import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Development.Duplo.Styles as Styles
import Development.Duplo.Utilities (logAction)
import Development.Shake
import Development.Shake.FilePath ((</>))
import System.Environment (lookupEnv, getArgs)
import Development.Duplo.ComponentIO
         ( appName
         , appVersion
         , appRepo
         , appId
         )
import Development.Duplo.Markups as Markups
import Development.Duplo.Scripts as Scripts
import Development.Duplo.Static as Static
import qualified Development.Duplo.Config as C

main :: IO ()
main = do
  -- Command-line arguments
  args <- getArgs
  let shakeCommand = head args

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
  let nodeModulesPath = duploPath </> "node_modules/.bin/"
  let utilPath        = duploPath </> "util/"
  let appPath         = cwd </> "app/"
  let devPath         = cwd </> "dev/"
  let assetsPath      = appPath </> "assets/"
  let targetPath      = cwd </> "public/"

  -- What to build
  let targetScript = targetPath </> "index.js"
  let targetStyle  = targetPath </> "index.css"
  let targetMarkup = targetPath </> "index.html"

  -- Gather information about this project
  appName'    <- appName
  appVersion' <- appVersion
  appId'      <- appId

  -- Report back what's given for confirmation
  putStr $ "\n"
        ++ ">> Parameters\n"
        ++ "Application name                   : "
        ++ appName' ++ "\n"
        ++ "Application version                : "
        ++ appVersion' ++ "\n"
        ++ "Component.IO repo ID               : "
        ++ appId' ++ "\n"
        ++ "Current working directory          : "
        ++ cwd ++ "\n"
        ++ "duplo is installed at              : "
        ++ duploPath ++ "\n"
        ++ "\n"
        ++ ">> Environment Variables\n"
        ++ "Runtime environment - `DUPLO_ENV`  : "
        ++ duploEnv ++ "\n"
        ++ "Build mode          - `DUPLO_MODE` : "
        ++ duploMode ++ "\n"
        ++ "App parameters      - `DUPLO_IN`   : "
        ++ duploIn ++ "\n"

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
                                  , C._utilPath   = utilPath
                                  , C._appPath    = appPath
                                  , C._devPath    = devPath
                                  , C._assetsPath = assetsPath
                                  , C._targetPath = targetPath
                                  }

  shake shakeOptions $ do
    targetScript *> Scripts.build buildConfig
    targetStyle  *> Styles.build cwd nodeModulesPath
    targetMarkup *> Markups.build cwd nodeModulesPath

    -- Manually bootstrap Shake
    action $ do
      need [shakeCommand]

    -- Handling static assets
    (Static.qualify buildConfig) &?> Static.build buildConfig

    "static" ~> Static.deps buildConfig

    "clean" ~> do
      -- Clean only when the target is there
      needCleaning <- doesDirectoryExist targetPath
      if   needCleaning
      then removeFilesAfter targetPath ["//*"]
      else return ()

      logAction "Clean completed"

    "version" ~> do
      return ()

    "bump" ~> do
      logAction "Bumping version"

    "build" ~> do
      need [ targetScript
           , targetStyle
           , targetMarkup
           ]
      -- Static files need to run after code compilation because they
      -- take precedence
      need [ "static"
           ]

      logAction "Build completed"
