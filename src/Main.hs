{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import System.Console.Byline
import Podcasts

chooseFeedMenu :: [PodcastShow] -> Menu Text
chooseFeedMenu shows = banner (text "Choose Feed") $ menu (uniqueFeedTitles shows) text

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

ignoreTextOutput :: Maybe Handle -> ExitCode -> IO ()
ignoreTextOutput _ _ = return ()

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

choiceToMaybe :: Choice a -> Maybe a
choiceToMaybe (Match a)   = Just a
choiceToMaybe _           = Nothing

askOptionally :: [a] -> (a -> Stylized) -> Stylized -> Stylized -> MaybeT (Byline IO) a
askOptionally menuChoices styling prompt errorMessage =
  let menuFromChoices = menu menuChoices styling
  in  MaybeT $ fmap choiceToMaybe $ askWithMenuRepeatedly menuFromChoices prompt errorMessage

stylizeShow :: PodcastShow -> Stylized
stylizeShow podcastShow = text $ showTitle podcastShow

choosePodcast :: [PodcastShow] -> IO (Maybe PodcastShow)
choosePodcast podcastShows = fmap join $ runByline $ runMaybeT $ do
  feedChoice <- askOptionally (uniqueFeedTitles podcastShows) text "Choose Feed" "You Need To Choose A Feed"
  let feedShows = sort $ filter (\s -> feedTitle s == feedChoice) podcastShows
  askOptionally feedShows stylizeShow "Choose Show" "You Need To Choose A Show"

playPodcast :: Text -> IO Bool
playPodcast podcastFile = do
  runProcessCreatingPipes ignoreTextOutput "git-annex" ["get", unpack podcastFile] ""
  runProcessNoPipes ignoreTextOutput "vlc" ["-I", "ncurses", "--no-loop", "--no-repeat", "--play-and-exit", unpack podcastFile] ""
  return True

setup :: Bool -> IO [PodcastShow]
setup clean = do
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
  return podcastShows

chooseAndPlay :: [PodcastShow] -> IO Bool
chooseAndPlay podcastShows = do
  podcastChosen <- choosePodcast podcastShows
  let podcastFile = fmap showFile podcastChosen
  let noShowChosen = putStrLn "No Show Chosen." >> return False
  maybe noShowChosen playPodcast podcastFile

main :: IO ()
main = do
  let parser = switch (long "clean" <> help "Clear out the database.")
  let opts = info parser mempty
  execParser opts >>= \clean -> do
    podcastShows <- setup clean
    let toLoop = chooseAndPlay podcastShows
    void $ iterateWhile id toLoop

