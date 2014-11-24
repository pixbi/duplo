{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (void, when, unless)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.ByteString.Base64 (decode)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromMaybe)
import Development.Duplo.Server (serve)
import Development.Duplo.Shake (shakeMain)
import Development.Duplo.Utilities (getProperty)
import Development.Duplo.Watcher (watch)
import Development.Shake (cmd)
import Development.Shake.FilePath ((</>))
import GHC.Conc (forkIO)
import System.Console.GetOpt (getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..))
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv, getArgs, getExecutablePath)
import System.Process (proc, createProcess, waitForProcess)
import qualified Control.Lens
import qualified Development.Duplo.Component as CM
import qualified Development.Duplo.Types.AppInfo as AI
import qualified Development.Duplo.Types.Config as TC
import qualified Development.Duplo.Types.Options as OP
import qualified Filesystem.Path
import qualified GHC.IO

main :: IO ()
main = do
  -- Command-line arguments
  args <- getArgs

  let (cmdName:cmdArgs) =
        if   (length args > 0)
        -- Normal case (with command name)
        then args
        -- Edge cases (with nothing provided)
        else ("":[])

  -- Deal with options
  let (actions, nonOptions, errors) = getOpt Permute OP.options args
  options <- foldl (>>=) (return OP.defaultOptions) actions

  -- Port for running dev server
  port      <- fmap read $ fromMaybe "8888" <$> lookupEnv "PORT"
  -- Environment - e.g. dev, staging, live
  duploEnv  <- fromMaybe "dev" <$> lookupEnv "DUPLO_ENV"
  -- Build mode, for dependency selection
  duploMode <- fromMaybe "" <$> lookupEnv "DUPLO_MODE"
  -- Application parameter
  duploIn'  <- fromMaybe "" <$> lookupEnv "DUPLO_IN"
  -- Current working directory
  cwd       <- getCurrentDirectory
  -- Duplo directory
  duploPath <- fmap (</> "../../../../") getExecutablePath

  -- Decode
  let duploIn = case (decode $ pack $ duploIn') of
                  Left _ -> ""
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
  appId <- getProperty CM.appId ""

  -- Internal command translation
  let duploEnv' = duploEnv
  let (cmdNameTranslated, bumpLevel, duploEnv, toWatch) =
        case cmdName of
          "info" -> ("version", "", duploEnv', False)
          "ver" -> ("version", "", duploEnv', False)
          "new" -> ("init", "", duploEnv', False)
          "bump" -> ("bump", "patch", duploEnv', False)
          "release" -> ("bump", "patch", duploEnv', False)
          "patch" -> ("bump", "patch", duploEnv', False)
          "minor" -> ("bump", "minor", duploEnv', False)
          "major" -> ("bump", "major", duploEnv', False)
          "dev" -> ("build", "", "dev", True)
          "live" -> ("build", "", "live", True)
          "test" -> ("build", "", "test", False)
          _ -> (cmdName, "", duploEnv', False)

  -- Certain flags turn into commands.
  let cmdNameWithFlags =
        if (OP.optVersion options)
          then "version"
          else cmdNameTranslated

  -- Display additional information when verbose.
  when (OP.optVerbose options) $
    -- More info about where we are
    putStr $ "\n"
          ++ ">> Current Directory\n"
          ++ "Application name                : "
          ++ appName ++ "\n"
          ++ "Application version             : "
          ++ appVersion ++ "\n"
          ++ "Component.IO repo ID            : "
          ++ appId ++ "\n"
          ++ "Current working directory       : "
          ++ cwd ++ "\n"
          ++ "duplo is installed at           : "
          ++ duploPath ++ "\n"
    -- Report back what's given for confirmation.
          ++ "\n"
          ++ ">> Environment Variables\n"
          ++ "DUPLO_ENV (runtime environment) : "
          ++ duploEnv ++ "\n"
          ++ "DUPLO_MODE (build mode)         : "
          ++ duploMode ++ "\n"
          ++ "DUPLO_IN (app parameters)       : "
          ++ duploIn ++ "\n"

  -- Display version either via command or option.
  when (cmdName == "version") $ do
    -- Prefacing space
    putStr "\n"

    let command = utilPath </> "display-version.sh"
    let process = createProcess $ proc command [duploPath]

    -- Run the command.
    (_, _, _, handle) <- process
    -- Remember to do it synchronously.
    waitForProcess handle

    return ()

  -- Construct environment
  let buildConfig = TC.BuildConfig { TC._appName      = appName
                                   , TC._appVersion   = appVersion
                                   , TC._appId        = appId
                                   , TC._cwd          = cwd
                                   , TC._duploPath    = duploPath
                                   , TC._env          = duploEnv
                                   , TC._mode         = duploMode
                                   , TC._nodejsPath   = nodeModulesPath
                                   , TC._dist         = distPath
                                   , TC._input        = duploIn
                                   , TC._utilPath     = utilPath
                                   , TC._miscPath     = miscPath
                                   , TC._defaultsPath = defaultsPath
                                   , TC._appPath      = appPath
                                   , TC._devPath      = devPath
                                   , TC._assetsPath   = assetsPath
                                   , TC._depsPath     = depsPath
                                   , TC._targetPath   = targetPath
                                   , TC._bumpLevel    = bumpLevel
                                   , TC._port         = port
                                   }

  -- Construct the Shake command
  let shake = shakeMain cmdNameTranslated cmdArgs buildConfig options

  -- Watch or just build
  unless toWatch shake
  when toWatch $ do
    -- Start a local server
    _ <- forkIO $ serve port

    -- Watch for file changes
    watch shake $ cwd </> "app/"
