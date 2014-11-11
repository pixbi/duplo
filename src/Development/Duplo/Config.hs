{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.Config
  ( BuildConfig(..)
  , isInDev
  , appName
  , appVersion
  , appId
  , cwd
  , duploPath
  , nodejsPath
  , env
  , mode
  , dist
  , input
  , utilPath
  , defaultsPath
  , appPath
  , devPath
  , assetsPath
  , depsPath
  , targetPath
  ) where

import Development.Shake
import Control.Lens hiding (Action)
import Control.Lens.TH (makeLenses)

data BuildConfig = BuildConfig { _appName      :: String
                               , _appVersion   :: String
                               , _appId        :: String
                               , _cwd          :: String
                               , _duploPath    :: FilePath
                               , _env          :: String
                               , _mode         :: String
                               , _dist         :: FilePath
                               , _input        :: String
                               , _utilPath     :: FilePath
                               , _nodejsPath   :: FilePath
                               , _defaultsPath :: FilePath
                               , _appPath      :: FilePath
                               , _devPath      :: FilePath
                               , _assetsPath   :: FilePath
                               , _depsPath     :: FilePath
                               , _targetPath   :: FilePath
                               } deriving (Show)

makeLenses ''BuildConfig

isInDev :: BuildConfig -> Bool
isInDev config = config ^. env == "dev"
