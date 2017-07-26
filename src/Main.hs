{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable
import Data.Maybe
import Data.Text hiding (lines, find, filter)
import Data.Text.Encoding
import Data.Text.Lens hiding (text)
import Turtle hiding (text, find, UTCTime)
import qualified Control.Foldl as F
import Data.Aeson.Lens
import Control.Lens hiding (Choice)
import Data.List
import System.Console.Byline
import System.Console.Byline.Menu
import Data.Time.Clock
import Data.Time.Format

data PodcastShow = PodcastShow
                 { lastChanged  :: Maybe UTCTime
                 , feedTitle    :: Maybe Text
                 , showTitle    :: Maybe Text
                 , showFile     :: Text
                 } deriving (Eq, Ord, Show)

feedTitleLens :: Lens' PodcastShow (Maybe Text)
feedTitleLens = lens feedTitle (\s -> \a -> s {feedTitle = a})

parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime utcValue = do
  let mainParts = splitOn "@" utcValue :: [Text]
  yearSegment <- mainParts ^? ix 0
  let yearParts = splitOn "-" yearSegment
  year <- yearParts ^? ix 0 . unpacked
  month <- yearParts ^? ix 1 . unpacked
  day <- yearParts ^? ix 2 . unpacked
  timeSegment <- mainParts ^? ix 1
  let timeParts = splitOn "-" timeSegment
  hour <- timeParts ^? ix 0 . unpacked
  minute <- timeParts ^? ix 1 . unpacked
  second <- timeParts ^? ix 2 . unpacked
  let timeElements = [('y', year), ('m', month), ('d', day), ('H', hour), ('M', minute), ('S', second)]
  buildTime defaultTimeLocale timeElements

parsePodcastShow :: Text -> Either String PodcastShow
parsePodcastShow value = maybe (Left ("Can't parse podcast: " ++ (unpack value))) return $ do
  pFile <- value ^? key "file" . _String
  let lChanged = value ^? key "fields" . key "lastchanged" . nth 0 . _String
  let sTitle = value ^? key "fields" . key "title" . nth 0 . _String
  let fTitle = value ^? key "fields" . key "feedtitle" . nth 0 . _String
  return $ PodcastShow (lChanged >>= parseUTCTime) fTitle sTitle pFile

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

choiceToMaybe :: Choice a -> Maybe a
choiceToMaybe (Match matched) = Just matched
choiceToMaybe _               = Nothing

choosePodcast :: [PodcastShow] -> Byline IO (Maybe PodcastShow)
choosePodcast podcastShows = do
  let feedTitles = uniqueFeedTitles podcastShows
  let feedMenu = menu feedTitles text
  feedChoice <- fmap choiceToMaybe $ askWithMenu feedMenu (text "Choose Feed")
  let feedShows = sort $ filter (\s -> feedTitle s == feedChoice) podcastShows
  let showMenu = menu (feedShows >>= (maybeToList . showTitle)) text
  showChoice <- fmap choiceToMaybe $ askWithMenu showMenu (text "Choose Show")
  let podcastShow = find (\s -> feedTitle s == feedChoice && showTitle s == showChoice) podcastShows
  return podcastShow

main :: IO ()
main = do
  podcastShows <- runGitAnnex
  print $ uniqueFeedTitles podcastShows
  podcastChosen <- fmap join $ runByline $ choosePodcast podcastShows
  let podcastFile = fmap showFile podcastChosen
  let noShowChosen = putStrLn "No Show Chosen."
  let playPodcast f = procs "vlc" [f] mempty
  maybe noShowChosen playPodcast podcastFile