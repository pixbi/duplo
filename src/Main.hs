import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Development.Duplo.Styles as Styles
import Development.Duplo.Utilities (logAction)
import Development.Shake
import Development.Shake.FilePath (combine)
{-import System.Directory (getCurrentDirectory)-}
import System.Environment (lookupEnv)
{-import System.Environment.Executable (splitExecutablePath)-}
{-import Data.String.Utils-}
import Development.Duplo.ComponentIO
         ( appName
         , appVersion
         , appRepo
         , appId
         )
{-import Development.Duplo.Files-}
import Development.Duplo.Markups as Markups
import Development.Duplo.Scripts as Scripts
{-import Development.Shake.Command-}
{-import Development.Shake.Util-}
{-import System.FSNotify (withManager, watchTree)-}
{-import Filesystem (getWorkingDirectory)-}
{-import Filesystem.Path (append)-}
{-import Filesystem.Path.CurrentOS (decodeString)-}
{-import Control.Concurrent (forkIO)-}
import qualified Development.Duplo.Config as C

main :: IO ()
main = do
  -- Environment - e.g. dev, staging, live
  duploEnv  <- fromMaybe "" <$> lookupEnv "DUPLO_ENV"
  -- Build mode, for dependency selection
  duploMode <- fromMaybe "" <$> lookupEnv "DUPLO_MODE"
  -- Application parameter
  duploIn   <- fromMaybe "" <$> lookupEnv "DUPLO_IN"
  -- Current directory
  cwd       <- fromMaybe "" <$> lookupEnv "CWD"
  -- Duplo directory
  duploPath <- fromMaybe "" <$> lookupEnv "DUPLO_PATH"

  -- Paths to various relevant directories
  let nodeModulesPath = combine duploPath "node_modules/.bin"
  let utilPath        = combine duploPath "util"

  -- What to build
  let target       = combine cwd "public/"
  let targetScript = combine target "index.js"
  let targetStyle  = combine target "index.css"
  let targetMarkup = combine target "index.html"

  -- Gather information about this project
  appName'    <- appName
  appVersion' <- appVersion
  appId'      <- appId

  -- Report back what's given for confirmation
  putStr $ ">> Parameters\n"
        ++ "Application name                  : "
        ++ appName' ++ "\n"
        ++ "Application version               : "
        ++ appVersion' ++ "\n"
        ++ "Component.IO repo ID              : "
        ++ appId' ++ "\n"
        ++ "Current working directory         : "
        ++ cwd ++ "\n"
        ++ "duplo is installed at             : "
        ++ duploPath ++ "\n"
        ++ "\n"
        ++ ">> Environment Variables\n"
        ++ "Runtime environment - `DUPLO_ENV` : "
        ++ duploEnv ++ "\n"
        ++ "Build mode - `DUPLO_MODE`         : "
        ++ duploMode ++ "\n"
        ++ "App parameters - `DUPLO_IN`       : "
        ++ duploIn ++ "\n"
        ++ "\n"

  -- Construct environment
  let buildConfig = C.BuildConfig { C._appName    = appName'
                                  , C._appVersion = appVersion'
                                  , C._appId      = appId'
                                  , C._cwd        = cwd
                                  , C._duploPath  = duploPath
                                  , C._env        = duploEnv
                                  , C._mode       = duploMode
                                  , C._bin        = utilPath
                                  , C._input      = duploIn
                                  }

  shakeArgs shakeOptions $ do
    want [targetScript, targetStyle, targetMarkup]

    -- Actions
    targetScript *> Scripts.build buildConfig
    targetStyle  *> Styles.build cwd nodeModulesPath
    targetMarkup *> Markups.build cwd nodeModulesPath

    "clean" ~> do
      logAction "Cleaning built files"
      cmd "rm" ["-rf", "public/"]

    "version" ~> do
      return ()

    "bump" ~> do
      logAction "Bumping version"

    "build" ~> do
      need [targetScript, targetStyle, targetMarkup]
