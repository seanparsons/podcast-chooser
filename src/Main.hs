{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text hiding (lines, find, filter)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Lens hiding (text)
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
import Control.Exception
import Control.Monad.Loops
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Options.Applicative
import Control.Concurrent.Async
import Control.Concurrent.QSem
import GHC.IO.Handle
import System.Process
import System.Exit

data PodcastShow = PodcastShow
                 { year         :: Maybe Int
                 , month        :: Maybe Int
                 , day          :: Maybe Int
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
    day_ <- field
    feedTitle_ <- field
    showTitle_ <- field
    showFile_ <- field
    return $ PodcastShowDBEntry (filePath_, (PodcastShow year_ month_ day_ feedTitle_ showTitle_ showFile_))

instance ToRow PodcastShowDBEntry where
  toRow (PodcastShowDBEntry (filePath_, (PodcastShow year_ month_ day_ feedTitle_ showTitle_ showFile_))) = toRow (filePath_, year_, month_, day_ , feedTitle_, showTitle_, showFile_)

feedTitleLens :: Lens' PodcastShow (Maybe Text)
feedTitleLens = lens feedTitle (\s -> \a -> s {feedTitle = a})

filterNewlines :: Text -> Text
filterNewlines = T.dropWhileEnd (\c -> c == '\n' || c == '\r')

parsePodcastShow :: Value -> Either String PodcastShow
parsePodcastShow value = maybe (Left ("Can't parse podcast: " ++ (show value))) return $ do
  pFile <- value ^? key "file" . _String
  let yearField = value ^? key "fields" . key "year" . nth 0 . _String . unpacked . decimal
  let monthField = value ^? key "fields" . key "month" . nth 0 . _String . unpacked . decimal
  let dayField = value ^? key "fields" . key "day" . nth 0 . _String . unpacked . decimal
  let sTitle = value ^? key "fields" . key "title" . nth 0 . _String
  let fTitle = value ^? key "fields" . key "feedtitle" . nth 0 . _String
  return $ PodcastShow yearField monthField dayField (fmap filterNewlines fTitle) (fmap filterNewlines sTitle) pFile

parseValue :: Text -> IO PodcastShow
parseValue text = either fail return $ do
  value <- eitherDecodeStrict $ encodeUtf8 text
  parsePodcastShow value

getAnnexMetadata :: Text -> IO Text
getAnnexMetadata path = do
  processOutput <- readCreateProcess ((proc "git-annex" ["metadata", "--json", unpack path]) { cwd = Just "/home/sean/Podcasts" }) ""
  return $ pack processOutput

runGitAnnex :: QSem -> Text -> IO PodcastShow
runGitAnnex semaphore path = bracket_ (waitQSem semaphore) (signalQSem semaphore) $ do
  metadataLine <- getAnnexMetadata path
  result <- parseValue metadataLine
  return result

uniqueFeedTitles :: [PodcastShow] -> [Text]
uniqueFeedTitles = nub . toListOf (traverse . feedTitleLens . _Just)

podcastsFolderWorkingDirectory :: CreateProcess -> CreateProcess
podcastsFolderWorkingDirectory cp = cp { cwd = Just "/home/sean/Podcasts" }

runThisProcess :: (CreateProcess -> CreateProcess) -> (Maybe Handle -> ExitCode -> IO a) -> String -> [String] -> String -> IO a
runThisProcess processTransform readOutput app params input = do
  let createProcess = processTransform $ proc app params
  withCreateProcess createProcess $ \possibleStdin possibleStdout _ processHandle -> do
    let debugDetails = Data.List.intercalate " " (app : params)
    forM_ possibleStdin $ \stdInHandle -> do
      -- Write to stdin.
      hPutStr stdInHandle input
      hFlush stdInHandle
    -- Wait for app to finish.
    exitCode <- waitForProcess processHandle
    -- Get output.
    readOutput possibleStdout exitCode

readTextOutput :: Maybe Handle -> ExitCode -> IO (Maybe Text)
readTextOutput _ (ExitFailure _)          = return Nothing
readTextOutput Nothing ExitSuccess        = fail "No stdout handle, can't read text output."
readTextOutput (Just handle) ExitSuccess  = do
  line <- hGetLine handle
  return $ Just $ pack line

ignoreTextOutput :: Maybe Handle -> ExitCode -> IO ()
ignoreTextOutput _ _ = return ()

runProcessInPodcastsFolder :: (Maybe Handle -> ExitCode -> IO a) -> String -> [String] -> String -> IO a
runProcessInPodcastsFolder = runThisProcess podcastsFolderWorkingDirectory

runProcessCreatingPipes :: (Maybe Handle -> ExitCode -> IO a) -> String -> [String] -> String -> IO a
runProcessCreatingPipes =
  let processTransform process = (podcastsFolderWorkingDirectory process) {
                                   std_in = CreatePipe,
                                   std_out = CreatePipe
                                 }
  in  runThisProcess processTransform

runProcessNoPipes :: (Maybe Handle -> ExitCode -> IO a) -> String -> [String] -> String -> IO a
runProcessNoPipes =
  let processTransform process = (podcastsFolderWorkingDirectory process) {
                                   std_in = Inherit,
                                   std_out = Inherit
                                 }
  in  runThisProcess processTransform

getChoice :: Text -> [Text] -> MaybeT IO Text
getChoice header choices = MaybeT $ do
  let input = join $ fmap (\t -> unpack t <> "\n") choices
  runProcessCreatingPipes readTextOutput "fzf" [unpack ("--header=" <> header), "--tiebreak=index", "--tac"] input

choosePodcast :: [PodcastShow] -> IO (Maybe PodcastShow)
choosePodcast podcastShows = runMaybeT $ do
  let feedTitles = uniqueFeedTitles podcastShows
  feedChoice <- getChoice "Choose Podcast" $ sort feedTitles
  let feedShows = sortOn (\p -> (year p, month p, day p, showTitle p)) $ filter (\s -> feedTitle s == Just feedChoice) podcastShows
  let showTitles = feedShows >>= (maybeToList . showTitle)
  showChoice <- getChoice "Choose Show" showTitles
  podcastShow <- MaybeT $ return $ find (\s -> feedTitle s == Just feedChoice && showTitle s == Just showChoice) podcastShows
  return podcastShow

playPodcast :: Text -> IO Bool
playPodcast podcastFile = do
  runProcessCreatingPipes ignoreTextOutput "git-annex" ["get", unpack podcastFile] ""
  runProcessNoPipes ignoreTextOutput "vlc" ["-I", "ncurses", "--no-loop", "--no-repeat", "--play-and-exit", unpack podcastFile] ""
  return True

getSymlinks :: IO [(Text, Text)]
getSymlinks = do
  let podcastsDir = [absdir|/home/sean/Podcasts|]
  (podcastDirs, _) <- listDir podcastsDir
  podcastFiles <- foldMap (fmap snd . listDir) podcastDirs
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
  execute_ connection "CREATE TABLE IF NOT EXISTS shows (filepath TEXT PRIMARY KEY NOT NULL, year INT NULL, month INT NULL, day INT NULL, feedtitle TEXT NULL, showtitle TEXT NULL, showfile TEXT)"
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

removeMissing :: Connection -> [PodcastShowDBEntry] -> IO ()
removeMissing connection toRemove = do
  traverse_ (\(PodcastShowDBEntry (r, _)) -> execute connection "DELETE FROM shows WHERE filepath = ?" (Only r)) toRemove

removeAll :: Connection -> IO ()
removeAll connection = execute_ connection "DELETE FROM shows"

addNew :: Connection -> [(Text, Text)] -> IO ()
addNew connection toAdd = do
  -- Retrieve metadata.
  semaphore <- newQSem 20
  newMetadata <- mapConcurrently (\(p, r) -> fmap (\m -> (PodcastShowDBEntry (r, m))) $ runGitAnnex semaphore p) toAdd
  --newMetadata <- traverse (\(p, r) -> fmap (\m -> (PodcastShowDBEntry (r, m))) $ runGitAnnex p) toAdd
  -- Add new metadata.
  traverse_ (\e -> execute connection "INSERT INTO shows VALUES (?, ?, ?, ?, ?, ?, ?)" e) newMetadata

chooseAndPlay :: Bool -> Connection -> IO Bool
chooseAndPlay clean connection = do
  -- Maybe clean the database away.
  when clean $ removeAll connection
  -- Get list of symlinks and real paths in Podcasts directory.
  symlinkPairs <- getSymlinks
  -- Get files in cache database.
  databaseShows <- getDatabaseShows connection
  let (toRemove, toAdd) = findToRemoveToAdd symlinkPairs databaseShows
  -- Remove entries not present in database.
  removeMissing connection toRemove
  -- For new entries.
  addNew connection toAdd
  -- Show menu.
  podcastShows <- (fmap . fmap) (\(PodcastShowDBEntry (_, e)) -> e) $ getDatabaseShows connection
  podcastChosen <- choosePodcast podcastShows
  let podcastFile = fmap showFile podcastChosen
  let noShowChosen = putStrLn "No Show Chosen." >> return False
  maybe noShowChosen playPodcast podcastFile

main :: IO ()
main = do
  let parser = switch (long "clean" <> help "Clear out the database.")
  let opts = info parser mempty
  execParser opts >>= \clean -> do
    let toLoop = bracket getConnection close $ chooseAndPlay clean
    void $ iterateWhile id toLoop

