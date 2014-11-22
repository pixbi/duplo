import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Development.Duplo.Shake (shakeMain)
import Development.Shake.FilePath ((</>))
import System.Environment (lookupEnv, getArgs)
import qualified Development.Duplo.ComponentIO as I
import qualified Development.Duplo.Types.Config as C
import Data.ByteString.Base64 (decode)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Development.Duplo.Types.AppInfo as AI
import Development.Duplo.Utilities (getProperty)

main :: IO ()
main = do
  -- Command-line arguments
  args <- getArgs
  let (cmd:cmdArgs) = args

  -- Environment - e.g. dev, staging, live
  duploEnv   <- fromMaybe "dev" <$> lookupEnv "DUPLO_ENV"
  -- Build mode, for dependency selection
  duploMode  <- fromMaybe "" <$> lookupEnv "DUPLO_MODE"
  -- Application parameter
  duploIn'   <- fromMaybe "" <$> lookupEnv "DUPLO_IN"
  -- Print extra info?
  verbose    <- fromMaybe "True" <$> lookupEnv "DUPLO_VERBOSE"
  -- Current directory
  cwd        <- fromMaybe "" <$> lookupEnv "CWD"
  -- Duplo directory
  duploPath  <- fromMaybe "" <$> lookupEnv "DUPLO_PATH"
  -- Semantic version bump level: patch, minor, or major
  bumpLevel <- fromMaybe "" <$> lookupEnv "DUPLO_BUMP_LEVEL"

  -- Decode
  let duploIn'' = decode $ pack duploIn'
  let duploIn   = case duploIn'' of
                    Left _      -> ""
                    Right input -> unpack input

  -- Paths to various relevant directories
  let nodeModulesPath = duploPath </> "node_modules/.bin/"
  let utilPath        = duploPath </> "util/"
  let distPath        = duploPath </> "dist/build/"
  let miscPath        = duploPath </> "etc/"
  let defaultsPath    = miscPath </> "static/"
  let appPath         = cwd </> "app/"
  let devPath         = cwd </> "dev/"
  let assetsPath      = appPath </> "assets/"
  let depsPath        = cwd </> "components/"
  let targetPath      = cwd </> "public/"

  -- Gather information about this project
  appName <- getProperty AI.name ""
  appVersion <- getProperty AI.version ""
  appId <- getProperty I.appId ""

  -- Report back what's given for confirmation
  let appInfo = "\n"
             ++ ">> Current Directory\n"
             ++ "Application name          : "
             ++ appName ++ "\n"
             ++ "Application version       : "
             ++ appVersion ++ "\n"
             ++ "Component.IO repo ID      : "
             ++ appId ++ "\n"
             ++ "Current working directory : "
             ++ cwd ++ "\n"
             ++ "duplo is installed at     : "
             ++ duploPath ++ "\n"
  let envInfo = "\n"
             ++ ">> Environment Variables\n"
             ++ "DUPLO_ENV (runtime environment) : "
             ++ duploEnv ++ "\n"
             ++ "DUPLO_MODE (build mode)         : "
             ++ duploMode ++ "\n"
             ++ "DUPLO_IN (app parameters)       : "
             ++ duploIn ++ "\n"

  -- Give it some space
  putStrLn "\n----------------------------------------"

  -- We don't always show the environment info, or the app info, depending
  -- on the mode.
  if   cmd == "version"
  then putStrLn appInfo
  else if   verbose == "True"
       then putStrLn $ appInfo ++ envInfo
       else putStrLn envInfo

  -- Construct environment
  let buildConfig = C.BuildConfig { C._appName      = appName
                                  , C._appVersion   = appVersion
                                  , C._appId        = appId
                                  , C._cwd          = cwd
                                  , C._duploPath    = duploPath
                                  , C._env          = duploEnv
                                  , C._mode         = duploMode
                                  , C._nodejsPath   = nodeModulesPath
                                  , C._dist         = distPath
                                  , C._input        = duploIn
                                  , C._utilPath     = utilPath
                                  , C._miscPath     = miscPath
                                  , C._defaultsPath = defaultsPath
                                  , C._appPath      = appPath
                                  , C._devPath      = devPath
                                  , C._assetsPath   = assetsPath
                                  , C._depsPath     = depsPath
                                  , C._targetPath   = targetPath
                                  , C._bumpLevel    = bumpLevel
                                  }

  shakeMain cmd cmdArgs buildConfig
