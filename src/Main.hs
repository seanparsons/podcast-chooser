{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable
import Data.Text hiding (lines)
import Data.Text.Encoding
import Turtle hiding (text)
import qualified Control.Foldl as F
import Data.Aeson.Lens
import Control.Lens
import Data.List
import System.Console.Byline
import System.Console.Byline.Menu


data PodcastShow = PodcastShow
                 { podcastFile   :: Text
                 , podcastTitle  :: Maybe Text
                 , feedTitle     :: Maybe Text
                 } deriving (Eq, Show)

feedTitleLens :: Lens' PodcastShow (Maybe Text)
feedTitleLens = lens feedTitle (\s -> \a -> s { feedTitle = a})

parsePodcastShow :: Text -> Either String PodcastShow
parsePodcastShow value = maybe (Left ("Can't parse podcast: " ++ (unpack value))) return $ do
  pFile <- value ^? key "file" . _String
  let pTitle = value ^? key "fields" . key "title" . nth 0 . _String
  let fTitle = value ^? key "fields" . key "feedtitle" . nth 0 . _String
  return $ PodcastShow pFile pTitle fTitle

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

choosePodcast :: [PodcastShow] -> Byline IO (Maybe PodcastShow)
choosePodcast podcastShows = do
  let feedTitles = uniqueFeedTitles podcastShows
  let feedMenu = menu feedTitles text
  feedChoice <- askWithMenu feedMenu (text "Choose Feed")
  liftIO $ print feedChoice
  return Nothing

main :: IO ()
main = do
  podcastShows <- runGitAnnex
  print $ uniqueFeedTitles podcastShows
  podcastChosen <- runByline $ choosePodcast podcastShows
  print podcastChosen