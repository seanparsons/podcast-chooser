{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Podcasts where

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

runGitAnnex :: QSem -> Text -> Text -> IO PodcastShow
runGitAnnex semaphore path symlink = bracket_ (waitQSem semaphore) (signalQSem semaphore) $ do
  metadataLine <- getAnnexMetadata path
  result <- parseValue metadataLine symlink
  return result

uniqueFeedTitles :: [PodcastShow] -> [Text]
uniqueFeedTitles = nub . toListOf (traverse . feedTitleLens . _Just)