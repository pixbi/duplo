{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.Types.Config where

import Development.Shake
import Control.Lens hiding (Action)
import Control.Lens.TH (makeLenses)
import Network.Wai.Handler.Warp (Port)

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
                               , _miscPath     :: FilePath
                               , _defaultsPath :: FilePath
                               , _appPath      :: FilePath
                               , _devPath      :: FilePath
                               , _assetsPath   :: FilePath
                               , _depsPath     :: FilePath
                               , _targetPath   :: FilePath
                               , _bumpLevel    :: String
                               , _port         :: Port
                               } deriving (Show)

makeLenses ''BuildConfig

isInDev :: BuildConfig -> Bool
isInDev config = config ^. env == "dev"