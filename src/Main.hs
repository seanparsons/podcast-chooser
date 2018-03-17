{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Lens hiding (Choice)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy (fromStrict)
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text hiding (lines, find, filter)
import Data.Text.Encoding
import Data.Text.Lens hiding (text)
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics
import GHC.IO.Handle
import Graphics.Vty.Menu
import Numeric.Lens
import Options.Applicative
import Path
import Path.IO
import Podcasts
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Exit
import System.Process
import Text.Printf

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

askOptionally :: (a -> String) -> [a] -> MaybeT IO a
askOptionally asString menuChoices = MaybeT $ displayMenuOfValues $ fmap (\a -> (asString a, a)) menuChoices

stylizeShow :: PodcastShow -> String
stylizeShow podcastShow =
  let baseTitle = showTitle podcastShow
      dmy = getDayMonthYear podcastShow
      withHyphen = fold $ fmap (\(d, m, y) -> printf "%02v/%02v/%04v - " d m y) dmy
  in  withHyphen `mappend` unpack baseTitle

askFeedChoice :: [Text] -> MaybeT IO Text
askFeedChoice = askOptionally unpack

askShowChoice :: [PodcastShow] -> MaybeT IO PodcastShow
askShowChoice = askOptionally stylizeShow

choosePodcast :: [PodcastShow] -> IO (Maybe PodcastShow)
choosePodcast podcastShows = runMaybeT $ do
  feedChoice <- askFeedChoice (sort $ uniqueFeedTitles podcastShows)
  let feedShows = sort $ filter (\s -> feedTitle s == feedChoice) podcastShows
  askShowChoice feedShows

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

