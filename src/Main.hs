{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BL
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
import GHC.Generics
import GHC.IO.Handle
import System.Process
import System.Exit

cacheFolder :: Path Abs Dir
cacheFolder = [absdir|/home/sean/.cache/podcast-chooser|]

cacheDBFile :: Path Abs File
cacheDBFile = cacheFolder </> [relfile|cache.db|]

podcastsDir :: Path Abs Dir
podcastsDir = [absdir|/home/sean/Podcasts|]

data PodcastShow = PodcastShow
                 { year         :: Maybe Int
                 , month        :: Maybe Int
                 , day          :: Maybe Int
                 , feedTitle    :: Maybe Text
                 , showTitle    :: Maybe Text
                 , showFile     :: Text
                 , symlinkPath  :: Text
                 } deriving (Eq, Ord, Show, Generic)

instance ToJSON PodcastShow where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PodcastShow

feedTitleLens :: Lens' PodcastShow (Maybe Text)
feedTitleLens = lens feedTitle (\s -> \a -> s {feedTitle = a})

filterNewlines :: Text -> Text
filterNewlines = T.dropWhileEnd (\c -> c == '\n' || c == '\r')

parsePodcastShow :: Value -> Text -> Either String PodcastShow
parsePodcastShow value symlinkPathField = maybe (Left ("Can't parse podcast: " ++ (show value))) return $ do
  pFile <- value ^? key "file" . _String
  let yearField = value ^? key "fields" . key "year" . nth 0 . _String . unpacked . decimal
  let monthField = value ^? key "fields" . key "month" . nth 0 . _String . unpacked . decimal
  let dayField = value ^? key "fields" . key "day" . nth 0 . _String . unpacked . decimal
  let sTitle = value ^? key "fields" . key "title" . nth 0 . _String
  let fTitle = value ^? key "fields" . key "feedtitle" . nth 0 . _String
  return $ PodcastShow yearField monthField dayField (fmap filterNewlines fTitle) (fmap filterNewlines sTitle) pFile symlinkPathField

parseValue :: Text -> Text -> IO PodcastShow
parseValue text symlinkPathField = either fail return $ do
  value <- eitherDecodeStrict $ encodeUtf8 text
  parsePodcastShow value symlinkPathField

getAnnexMetadata :: Text -> IO Text
getAnnexMetadata path = do
  processOutput <- readCreateProcess ((proc "git-annex" ["metadata", "--json", unpack path]) { cwd = Just $ toFilePath podcastsDir }) ""
  return $ pack processOutput

runGitAnnex :: QSem -> Text -> Text -> IO PodcastShow
runGitAnnex semaphore path symlink = bracket_ (waitQSem semaphore) (signalQSem semaphore) $ do
  metadataLine <- getAnnexMetadata path
  result <- parseValue metadataLine symlink
  return result

uniqueFeedTitles :: [PodcastShow] -> [Text]
uniqueFeedTitles = nub . toListOf (traverse . feedTitleLens . _Just)

podcastsFolderWorkingDirectory :: CreateProcess -> CreateProcess
podcastsFolderWorkingDirectory cp = cp { cwd = Just $ toFilePath podcastsDir }

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
  (podcastDirs, _) <- listDir podcastsDir
  podcastFiles <- foldMap (fmap snd . listDir) podcastDirs
  podcastSymlinks <- filterM isSymlink podcastFiles
  pathPairs <- traverse (\p -> fmap (\r -> (p, r)) $ canonicalizePath p) podcastSymlinks
  return $ fmap (\(p, r) -> (pack $ toFilePath p, pack $ toFilePath r)) pathPairs

removeAll :: IO ()
removeAll = do
  exists <- doesFileExist cacheDBFile
  when exists $ removeFile cacheDBFile

findToRemoveToAdd :: [(Text, Text)] -> [PodcastShow] -> ([PodcastShow], [(Text, Text)])
findToRemoveToAdd symlinkPairs podcastEntries =
  let resolvedSymlinkPaths = fmap snd symlinkPairs
      toRemove = filter (\r -> notElem (symlinkPath r) resolvedSymlinkPaths) podcastEntries
      entryPaths = fmap symlinkPath podcastEntries
      toAdd = filter (\(_, r) -> notElem r entryPaths) symlinkPairs
  in  (toRemove, toAdd)

getNew :: [(Text, Text)] -> IO [PodcastShow]
getNew toAdd = do
  -- Retrieve metadata.
  semaphore <- newQSem 20
  mapConcurrently (\(p, r) -> runGitAnnex semaphore p r) toAdd

getCachedShows :: IO [PodcastShow]
getCachedShows = do
  exists <- doesFileExist cacheDBFile
  case exists of
    False   -> return []
    True    -> do
      cacheFileContents <- BL.readFile $ toFilePath cacheDBFile
      let decodeResult = eitherDecode' cacheFileContents
      either fail return decodeResult

writeCache :: [PodcastShow] -> IO ()
writeCache shows = do
  createDirIfMissing True cacheFolder
  let encoded = encode shows
  BL.writeFile (toFilePath cacheDBFile) encoded 

chooseAndPlay :: Bool -> IO Bool
chooseAndPlay clean = do
  -- Maybe clean the cache away.
  when clean $ removeAll
  -- Get list of symlinks and real paths in Podcasts directory.
  symlinkPairs <- getSymlinks
  -- Get files in cache database.
  cachedShows <- getCachedShows
  let (toRemove, toAdd) = findToRemoveToAdd symlinkPairs cachedShows
  newShows <- getNew toAdd
  -- Handle the changes.
  let podcastShows = (cachedShows \\ toRemove) ++ newShows
  writeCache podcastShows
  podcastChosen <- choosePodcast podcastShows
  let podcastFile = fmap showFile podcastChosen
  let noShowChosen = putStrLn "No Show Chosen." >> return False
  maybe noShowChosen playPodcast podcastFile

main :: IO ()
main = do
  let parser = switch (long "clean" <> help "Clear out the database.")
  let opts = info parser mempty
  execParser opts >>= \clean -> do
    let toLoop = chooseAndPlay clean
    void $ iterateWhile id toLoop

