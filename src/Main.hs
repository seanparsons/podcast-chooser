{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable
import Data.Maybe
import Data.Text hiding (lines, find, filter)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Lens hiding (text)
import Turtle hiding (text, find, UTCTime, decimal, (</>))
import qualified Control.Foldl as F
import Data.Aeson.Lens
import Control.Lens hiding (Choice)
import Numeric.Lens
import Data.List
import Data.Time.Clock
import Data.Time.Format
import Database.SQLite.Simple
import Path
import Path.IO
import Control.Monad.Extra

data PodcastShow = PodcastShow
                 { year         :: Maybe Int
                 , month        :: Maybe Int
                 , feedTitle    :: Maybe Text
                 , showTitle    :: Maybe Text
                 , showFile     :: Text
                 } deriving (Eq, Ord, Show)

newtype PodcastShowDBEntry = PodcastShowDBEntry (Text, PodcastShow) deriving (Eq, Ord, Show)

instance FromRow PodcastShowDBEntry where
  fromRow = do
    filePath_ <- field
    year_ <- field
    month_ <- field
    feedTitle_ <- field
    showTitle_ <- field
    showFile_ <- field
    return $ PodcastShowDBEntry (filePath_, (PodcastShow year_ month_ feedTitle_ showTitle_ showFile_))

instance ToRow PodcastShowDBEntry where
  toRow (PodcastShowDBEntry (filePath_, (PodcastShow year_ month_ feedTitle_ showTitle_ showFile_))) = toRow (filePath_, year_, month_, feedTitle_, showTitle_, showFile_)

feedTitleLens :: Lens' PodcastShow (Maybe Text)
feedTitleLens = lens feedTitle (\s -> \a -> s {feedTitle = a})

parsePodcastShow :: Text -> Either String PodcastShow
parsePodcastShow value = maybe (Left ("Can't parse podcast: " ++ (unpack value))) return $ do
  pFile <- value ^? key "file" . _String
  let yearField = value ^? key "fields" . key "year" . nth 0 . _String . unpacked . decimal
  let monthField = value ^? key "fields" . key "month" . nth 0 . _String . unpacked . decimal
  let sTitle = value ^? key "fields" . key "title" . nth 0 . _String
  let fTitle = value ^? key "fields" . key "feedtitle" . nth 0 . _String
  return $ PodcastShow yearField monthField fTitle sTitle pFile

parseValue :: Line -> IO PodcastShow
parseValue line = either fail return $ parsePodcastShow $ lineToText line

getAnnexMetadata :: Text -> Shell Line
getAnnexMetadata path = do
  cd "/home/sean/Podcasts"
  inproc "git-annex" ["metadata", "--json", path] mempty

runGitAnnex :: Text -> IO PodcastShow
runGitAnnex path = do
  possibleShow <- (flip foldIO) (F.generalize F.head) $ do
    metadataLine <- getAnnexMetadata path
    liftIO $ parseValue metadataLine
  maybe (fail ("No show: " ++ unpack path)) return possibleShow

uniqueFeedTitles :: [PodcastShow] -> [Text]
uniqueFeedTitles = nub . toListOf (traverse . feedTitleLens . _Just)

getChoice :: Text -> [Text] -> IO (Maybe Text)
getChoice header choices = do
  let inputLines = choices >>= (maybeToList . textToLine)
  let input = select inputLines
  (exitCode, result) <- procStrict "fzf" ["--header=" `mappend` header, "--tiebreak=index", "--tac"] input
  return $ Just $ T.filter (/= '\n') result

choosePodcast :: [PodcastShow] -> IO (Maybe PodcastShow)
choosePodcast podcastShows = do
  let feedTitles = uniqueFeedTitles podcastShows
  feedChoice <- getChoice "Choose Podcast" feedTitles
  let feedShows = sortOn (\p -> (year p, month p, showTitle p)) $ filter (\s -> feedTitle s == feedChoice) podcastShows
  let showTitles = feedShows >>= (maybeToList . showTitle)
  showChoice <- getChoice "Choose Show" showTitles
  let podcastShow = find (\s -> feedTitle s == feedChoice && showTitle s == showChoice) podcastShows
  return podcastShow

playPodcast :: Text -> IO ()
playPodcast podcastFile = do
  procs "git-annex" ["get", podcastFile] mempty
  procs "vlc" [podcastFile] mempty

getSymlinks :: IO [(Text, Text)]
getSymlinks = do
  let podcastsDir = [absdir|/home/sean/Podcasts|]
  (podcastDirs, _) <- listDir podcastsDir
  podcastFiles <- foldMap (fmap snd . listDir) podcastDirs
  print podcastFiles
  podcastSymlinks <- filterM isSymlink podcastFiles
  pathPairs <- traverse (\p -> fmap (\r -> (p, r)) $ canonicalizePath p) podcastSymlinks
  return $ fmap (\(p, r) -> (pack $ toFilePath p, pack $ toFilePath r)) pathPairs

getConnection :: IO Connection
getConnection = do
  let cacheFolder = [absdir|/home/sean/.cache/podcast-chooser|]
  -- Create cache folder.
  createDirIfMissing True cacheFolder
  -- Touch the file if it doesn't exist.
  let cacheDBFile = cacheFolder </> [relfile|cache.db|]
  -- Open database connection.
  connection <- open (toFilePath cacheDBFile)
  -- Create SHOWS table.
  execute_ connection "CREATE TABLE IF NOT EXISTS shows (filepath TEXT PRIMARY KEY NOT NULL, year INT NULL, month INT NULL, feedtitle TEXT NULL, showtitle TEXT NULL, showfile TEXT)"
  return connection

getDatabaseShows :: Connection -> IO [PodcastShowDBEntry]
getDatabaseShows connection = query_ connection "SELECT * FROM shows"

findToRemoveToAdd :: [(Text, Text)] -> [PodcastShowDBEntry] -> ([PodcastShowDBEntry], [(Text, Text)])
findToRemoveToAdd symlinkPairs podcastEntries =
  let resolvedSymlinkPaths = fmap snd symlinkPairs
      entryPaths = fmap (\(PodcastShowDBEntry (r, _)) -> r) podcastEntries
      toRemove = filter (\(PodcastShowDBEntry (r, _)) -> notElem r resolvedSymlinkPaths) podcastEntries
      toAdd = filter (\(_, r) -> notElem r entryPaths) symlinkPairs
  in  (toRemove, toAdd)

main :: IO ()
main = do
  connection <- getConnection
  -- Get list of symlinks and real paths in Podcasts directory.
  symlinkPairs <- getSymlinks
  --print symlinkPairs
  -- Get files in cache database.
  databaseShows <- getDatabaseShows connection
  let (toRemove, toAdd) = findToRemoveToAdd symlinkPairs databaseShows
  --print toRemove
  --print toAdd
  -- Remove entries not present in database.
  traverse (\(PodcastShowDBEntry (r, _)) -> execute connection "DELETE FROM shows WHERE filepath = ?" (Only r)) toRemove
  -- For new entries:
    -- Retrieve metadata.
  newMetadata <- traverse (\(p, r) -> fmap (\m -> (PodcastShowDBEntry (r, m))) $ runGitAnnex p) toAdd
    -- Add new metadata.
  traverse_ (\e -> execute connection "INSERT INTO shows VALUES (?, ?, ?, ?, ?, ?)" e) newMetadata
  -- Show menu.
  podcastShows <- (fmap . fmap) (\(PodcastShowDBEntry (_, e)) -> e) $ getDatabaseShows connection
  podcastChosen <- choosePodcast podcastShows
  let podcastFile = fmap showFile podcastChosen
  let noShowChosen = putStrLn "No Show Chosen."
  maybe noShowChosen playPodcast podcastFile
  -- Close connection.
  close connection

