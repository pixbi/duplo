{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.Config
  ( BuildConfig(..)
  , appName
  , appVersion
  , appId
  , cwd
  , duploPath
  , env
  , mode
  , bin
  , input
  , utilPath
  , appPath
  , assetsPath
  , targetPath
  ) where

import Development.Shake
import Control.Lens.TH (makeLenses)

data BuildConfig = BuildConfig { _appName    :: String
                               , _appVersion :: String
                               , _appId      :: String
                               , _cwd        :: String
                               , _duploPath  :: FilePath
                               , _env        :: String
                               , _mode       :: String
                               , _bin        :: FilePath
                               , _input      :: String
                               , _utilPath   :: FilePath
                               , _appPath    :: FilePath
                               , _assetsPath :: FilePath
                               , _targetPath :: FilePath
                               }

makeLenses ''BuildConfig
