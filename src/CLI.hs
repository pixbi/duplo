{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>))
import Control.Exception (catch, handle, throw, Exception)
import Control.Lens.Operators
import Control.Monad (void, when, unless)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.ByteString.Base64 (decode)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromMaybe)
import Development.Duplo.Server (serve)
import Development.Duplo.Utilities (logStatus, errorPrintSetter, replace)
import Development.Duplo.Watcher (watch)
import Development.Shake (cmd, ShakeException(..))
import Development.Shake.FilePath ((</>))
import Duplo (build)
import GHC.Conc (forkIO)
import System.Console.GetOpt (getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..))
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv, getArgs, getExecutablePath)
import System.FilePath.Posix (takeDirectory)
import System.Process (proc, createProcess, waitForProcess)
import qualified Control.Lens
import qualified Development.Duplo.Component as CM
import qualified Development.Duplo.Types.AppInfo as AI
import qualified Development.Duplo.Types.Builder as TB
import qualified Development.Duplo.Types.Config as TC
import qualified Development.Duplo.Types.Options as OP
import qualified Filesystem.Path
import qualified GHC.IO

main :: IO ()
main = do
  -- Command-line arguments
  args <- getArgs

  let (cmdName:cmdArgs) =
        if   not (null args)
        -- Normal case (with command name)
        then args
        -- Edge cases (with nothing provided)
        else [""]

  -- Deal with options
  let (actions, nonOptions, errors) = getOpt Permute OP.options args
  options <- foldl (>>=) (return OP.defaultOptions) actions

  -- Port for running dev server
  port       <- fmap read $ fromMaybe "8888" <$> lookupEnv "PORT"
  -- Environment - e.g. dev, staging, production
  duploEnvMB <- lookupEnv "DUPLO_ENV"
  -- Build mode, for dependency selection
  duploMode  <- fromMaybe "" <$> lookupEnv "DUPLO_MODE"
  -- Application parameter
  duploIn    <- fromMaybe "" <$> lookupEnv "DUPLO_IN"
  -- Current working directory
  cwd        <- getCurrentDirectory
  -- Duplo directory, assuming this is a build cabal executable (i.e.
  -- `./dist/build/duplo/duplo`)
  duploPath  <- fmap ((</> "../../../") . takeDirectory) getExecutablePath

  -- Base64 decode
  let duploInDecoded = case decode $ pack duploIn of
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
  let testPath        = cwd </> "test/"
  let assetsPath      = appPath </> "assets/"
  let depsPath        = cwd </> "components/"
  let targetPath      = cwd </> "public/"

  -- Extract environment
  let duploEnv = case cmdName of
                    -- `build` is a special case. It takes `production` as the
                    -- default.
                    "build" -> fromMaybe "production" duploEnvMB
                    -- By default, `dev` is the default.
                    _ -> fromMaybe "development" duploEnvMB

  -- Internal command translation
  let (cmdNameTranslated, bumpLevel, buildMode, toWatch) =
        case cmdName of
          "info"       -> ( "version" , ""      , duploEnv      , False )
          "ver"        -> ( "version" , ""      , duploEnv      , False )
          "new"        -> ( "init"    , ""      , duploEnv      , False )
          "bump"       -> ( "bump"    , "patch" , duploEnv      , False )
          "release"    -> ( "bump"    , "patch" , duploEnv      , False )
          "patch"      -> ( "bump"    , "patch" , duploEnv      , False )
          "minor"      -> ( "bump"    , "minor" , duploEnv      , False )
          "major"      -> ( "bump"    , "major" , duploEnv      , False )
          "dev"        -> ( "build"   , ""      , "development" , True  )
          "live"       -> ( "build"   , ""      , "production"  , True  )
          "production" -> ( "build"   , ""      , "production"  , True  )
          "build"      -> ( "build"   , ""      , duploEnv      , False )
          "test"       -> ( "build"   , ""      , "test"        , False )
          _            -> ( cmdName   , ""      , duploEnv      , False )

  -- Certain flags turn into commands.
  let cmdNameWithFlags = if   OP.optVersion options
                         then "version"
                         else cmdNameTranslated

  -- Display version either via command or option. We need to do this
  -- before any `readManifest` as it throws an error when there isn't one,
  -- as it should.
  when (cmdNameWithFlags == "version") $ do
    let command = utilPath </> "display-version.sh"
    let process = createProcess $ proc command [duploPath]

    -- Run the command.
    (_, _, _, handle) <- process
    -- Remember to do it synchronously.
    void $ waitForProcess handle

  -- We only care about exceptions thrown by the builder.
  let ignoreManifestError' (e :: TB.BuilderException) = case e of
        -- Only when missing manifest
        TB.MissingManifestException _ -> return ""
        -- Re-throw other builder exceptions.
        _ -> throw e
  -- Helper function to ignore exceptions, only for this stage, before
  -- Shake is run.
  let ignoreManifestError io = catch io ignoreManifestError'

  -- Gather information about this project
  appName <- ignoreManifestError $ fmap AI.name CM.readManifest
  appVersion <- ignoreManifestError $ fmap AI.version CM.readManifest
  appId <- ignoreManifestError $ fmap CM.appId CM.readManifest

  -- We may need custom builds with mode
  let depManifestPath = cwd </> "component.json"
  dependencies <- CM.getDependencies $ case duploMode of
                                         "" -> Nothing
                                         a  -> Just a
  let depIds = fmap (replace "/" "-") dependencies

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
          ++ duploInDecoded ++ "\n"

  -- Construct environment
  let buildConfig = TC.BuildConfig
                  { TC._appName      = appName
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
                  , TC._testPath     = testPath
                  , TC._assetsPath   = assetsPath
                  , TC._depsPath     = depsPath
                  , TC._targetPath   = targetPath
                  , TC._bumpLevel    = bumpLevel
                  , TC._port         = port
                  , TC._dependencies = depIds
                  , TC._buildMode    = buildMode
                  }

  -- If there is a Makefile, run that as well, with the environment as the
  -- target (e.g. `duplo dev` would run `make development` and `duplo build` would
  -- run `make production`).
  makefileExists <- doesFileExist $ cwd </> "Makefile"
  when makefileExists $ void $ createProcess $ proc "make" [duploEnv]

  -- Construct the Shake command.
  let shake' = build  cmdNameWithFlags cmdArgs buildConfig options
  let shake  = shake' `catch` handleExc

  -- Watch or just build.
  unless toWatch shake
  when toWatch $ do
    -- Start a local server.
    _ <- forkIO $ serve port

    -- Only watch the dev and the app directories. We're not watching the
    -- dependency directory because it triggers a race condition with
    -- componentjs.
    let targetDirs = [devPath, appPath]
    -- Make sure we have these directories to watch.
    mapM_ (createDirectoryIfMissing True) targetDirs
    -- Watch for file changes.
    watch shake targetDirs

-- | Handle all errors.
handleExc (e :: ShakeException) = do
    putStr $ show e
    logStatus errorPrintSetter "Build failed"
    putStrLn ""
