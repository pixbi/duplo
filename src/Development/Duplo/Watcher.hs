{-# LANGUAGE OverloadedStrings #-}

module Development.Duplo.Watcher
  ( watch
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, when, unless)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Maybe (isJust, fromJust)
import Data.String (fromString)
import GHC.Conc (ThreadId(..), forkIO, killThread)
import System.FSNotify (withManagerConf, watchTree, WatchConfig(..), Debounce(..), Action, Event)
import System.FilePath.Posix (FilePath)

-- | Given the path to watch and something to do, watch every 100ms without
-- debouncing but would interrupt the action when there is a new event.
watch :: IO () -> FilePath -> IO ()
watch onChange path = do
    let watchConfig = WatchConfig { confDebounce = NoDebounce
                                  , confPollInterval = 100000
                                  , confUsePolling = False
                                  }

    tidVar <- newIORef (Nothing :: Maybe ThreadId)

    withManagerConf watchConfig $ \manager -> do
      let path' = fromString path
      let always = const True
      let handler = handleEvent tidVar onChange

      -- Always start an initial round
      onChange

      -- Watch for changes
      watchTree manager path' always handler

      -- Hibernate!
      forever $ threadDelay $ 1000000 * 60 * 60 * 24 * 365

-- | Interrupt the given thread and restart the "callback".
handleEvent :: IORef (Maybe ThreadId) -> IO () -> Action
handleEvent tidVar onChange _ = do
    tid <- readIORef tidVar

    -- Kill existing thread
    when (isJust tid) $ killThread $ fromJust tid

    -- Perform action
    newTid <- forkIO onChange

    -- Save thread ID
    writeIORef tidVar $ Just newTid
