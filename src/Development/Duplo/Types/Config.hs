{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.Types.Config where

import           Control.Lens             hiding (Action)
import           Control.Lens.TH          (makeLenses)
import           Development.Shake
import           Network.Wai.Handler.Warp (Port)

data BuildConfig = BuildConfig { _appName      :: String
                               , _appVersion   :: String
                               , _appId        :: String
                               , _cwd          :: String
                               , _duploPath    :: FilePath
                               -- The "environment" that can be queried at
                               -- runtime.
                               , _env          :: String
                               -- Instruct duplo what to build.
                               , _mode         :: String
                               , _dist         :: FilePath
                               , _input        :: String
                               , _utilPath     :: FilePath
                               , _nodejsPath   :: FilePath
                               , _miscPath     :: FilePath
                               , _defaultsPath :: FilePath
                               , _appPath      :: FilePath
                               , _devPath      :: FilePath
                               , _testPath     :: FilePath
                               , _assetsPath   :: FilePath
                               , _depsPath     :: FilePath
                               , _targetPath   :: FilePath
                               , _bumpLevel    :: String
                               , _port         :: Port
                               , _dependencies :: [String]
                               -- `dev`, `test`, or otherwise. Only
                               -- concerns duplo.
                               , _buildMode    :: String
                               } deriving (Show)

makeLenses ''BuildConfig
