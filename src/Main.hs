{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable
import Data.Maybe
import Data.Text hiding (lines, find, filter)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Lens hiding (text)
import Turtle hiding (text, find, UTCTime, decimal)
import qualified Control.Foldl as F
import Data.Aeson.Lens
import Control.Lens hiding (Choice)
import Numeric.Lens
import Data.List
import Data.Time.Clock
import Data.Time.Format

data PodcastShow = PodcastShow
                 { year         :: Maybe Int
                 , month        :: Maybe Int
                 , feedTitle    :: Maybe Text
                 , showTitle    :: Maybe Text
                 , showFile     :: Text
                 } deriving (Eq, Ord, Show)

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

parseValue :: Text -> IO [PodcastShow]
parseValue value =
  let parseResult = parsePodcastShow value
  in  either fail (return . return) parseResult

gitAnnexShell :: Shell Line
gitAnnexShell = do
  cd "/home/sean/Podcasts"
  inproc "git-annex" ["metadata", "--json", "."] mempty

runGitAnnex :: IO [PodcastShow]
runGitAnnex = foldIO gitAnnexShell (F.premapM lineToText $ F.sink parseValue)

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

main :: IO ()
main = do
  podcastShows <- runGitAnnex
  podcastChosen <- choosePodcast podcastShows
  let podcastFile = fmap showFile podcastChosen
  let noShowChosen = putStrLn "No Show Chosen."
  maybe noShowChosen playPodcast podcastFile