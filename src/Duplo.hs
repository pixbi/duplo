import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Development.Duplo.Utilities (logAction)
import Development.Shake
import Development.Shake.FilePath ((</>))
import System.Environment (lookupEnv, getArgs)
import qualified Development.Duplo.ComponentIO as I
import Development.Duplo.Markups as Markups
import Development.Duplo.Styles as Styles
import Development.Duplo.Scripts as Scripts
import Development.Duplo.Static as Static
import Development.Duplo.Git as Git
import qualified Development.Duplo.Config as C
import Control.Lens hiding (Action)
import Data.ByteString.Base64 (decode)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad (void)
import qualified Development.Duplo.Types.AppInfo as AI

main :: IO ()
main = do
  -- Command-line arguments
  args <- getArgs
  let (cmd:cmdArgs) = args

  -- Environment - e.g. dev, staging, live
  duploEnv   <- fromMaybe "" <$> lookupEnv "DUPLO_ENV"
  -- Build mode, for dependency selection
  duploMode  <- fromMaybe "" <$> lookupEnv "DUPLO_MODE"
  -- Application parameter
  duploIn'   <- fromMaybe "" <$> lookupEnv "DUPLO_IN"
  -- Print extra info?
  verbose    <- fromMaybe "" <$> lookupEnv "DUPLO_VERBOSE"
  -- Current directory
  cwd        <- fromMaybe "" <$> lookupEnv "CWD"
  -- Duplo directory
  duploPath  <- fromMaybe "" <$> lookupEnv "DUPLO_PATH"
  -- Semantic version bump level: patch, minor, or major
  bumpLevel' <- fromMaybe "" <$> lookupEnv "DUPLO_BUMP_LEVEL"
  let bumpLevel = if   bumpLevel' `elem` ["patch", "minor", "major"]
                  then bumpLevel'
                  else "patch"

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

  -- What to build
  let targetScript = targetPath </> "index.js"
  let targetStyle  = targetPath </> "index.css"
  let targetMarkup = targetPath </> "index.html"

  -- Gather information about this project
  appNameMaybe    <- runMaybeT $ getProperty AI.name
  appVersionMaybe <- runMaybeT $ getProperty AI.version
  appIdMaybe      <- runMaybeT $ getProperty I.appId
  let appName'     = maybe "" id appNameMaybe
  let appVersion'  = maybe "" id appVersionMaybe
  let appId'       = maybe "" id appIdMaybe

  -- Report back what's given for confirmation
  let appInfo = "\n"
             ++ ">> Current Directory\n"
             ++ "Application name          : "
             ++ appName' ++ "\n"
             ++ "Application version       : "
             ++ appVersion' ++ "\n"
             ++ "Component.IO repo ID      : "
             ++ appId' ++ "\n"
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
  else if   verbose == "true"
       then putStrLn $ appInfo ++ envInfo
       else putStrLn envInfo

  -- Construct environment
  let buildConfig = C.BuildConfig { C._appName      = appName'
                                  , C._appVersion   = appVersion'
                                  , C._appId        = appId'
                                  , C._cwd          = cwd
                                  , C._duploPath    = duploPath
                                  , C._env          = duploEnv
                                  , C._mode         = duploMode
                                  , C._nodejsPath   = nodeModulesPath
                                  , C._dist         = distPath
                                  , C._input        = duploIn
                                  , C._utilPath     = utilPath
                                  , C._defaultsPath = defaultsPath
                                  , C._appPath      = appPath
                                  , C._devPath      = devPath
                                  , C._assetsPath   = assetsPath
                                  , C._depsPath     = depsPath
                                  , C._targetPath   = targetPath
                                  }

  shake shakeOptions $ do
    targetScript *> (void . runMaybeT . Scripts.build buildConfig)
    targetStyle  *> (void . runMaybeT . Styles.build buildConfig)
    targetMarkup *> (void . runMaybeT . Markups.build buildConfig)

    -- Manually bootstrap Shake
    action $ do
      need [cmd]

    -- Handling static assets
    (Static.qualify buildConfig) &?> Static.build buildConfig

    "static" ~> Static.deps buildConfig

    "clean" ~> do
      -- Clean only when the target is there
      needCleaning <- doesDirectoryExist targetPath
      if   needCleaning
      then liftIO $ removeFiles targetPath ["//*"]
      else return ()

      logAction "Clean completed"

    "version" ~> do
      -- Version information should already have
      return ()

    "build" ~> do
      -- Always rebuild if we're building for production.
      if   C.isInDev buildConfig
      then return ()
      else need ["clean"]

      -- Copy over static files first
      need ["static"]
      -- Then compile
      need [targetScript, targetStyle, targetMarkup]

      logAction "Build completed"

    "bump" ~> do
      (oldVersion, newVersion) <- Git.commit buildConfig bumpLevel

      logAction $ "Bumped version from " ++ oldVersion ++ " to " ++ newVersion

    "init" ~> do
      let user = cmdArgs ^. element 0
      let repo = cmdArgs ^. element 1
      let name = user ++ "/" ++ repo
      let src  = miscPath </> "boilerplate/"
      let dest = cwd ++ "/"

      logAction $ "Creating new duplo project " ++ name

      -- Initialize with boilerplate
      command_ [] (utilPath </> "init-boilerplate.sh") [src, dest]

      -- Update fields
      Just appInfo <- liftIO $ runMaybeT $ I.readManifest
      let newAppInfo = appInfo { AI.name = repo
                               , AI.repo = name
                               }
      -- Commit app info
      liftIO $ I.writeManifest newAppInfo

      -- Initalize git
      command_ [] (utilPath </> "init-git.sh") [name]

      logAction $ "Project created at " ++ dest

-- | Get a particular manifest property property
-- | TODO: use Lens?
getProperty :: (AI.AppInfo -> a) -> MaybeT IO a
getProperty accessor = do
    appInfo <- I.readManifest
    return $ accessor appInfo
