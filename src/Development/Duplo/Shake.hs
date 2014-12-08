module Development.Duplo.Shake
  ( shakeMain
  ) where

import Control.Exception (throw)
import Control.Lens hiding (Action)
import Control.Monad (void)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Development.Duplo.Git as Git
import Development.Duplo.Markups as Markups
import Development.Duplo.Scripts as Scripts
import Development.Duplo.Static as Static
import Development.Duplo.Styles as Styles
import Development.Duplo.Utilities (logAction)
import Development.Shake
import Development.Shake.FilePath ((</>))
import System.Console.GetOpt (OptDescr(..), ArgDescr(..))
import System.IO (readFile)
import qualified Development.Duplo.Component as CM
import qualified Development.Duplo.Types.AppInfo as AI
import qualified Development.Duplo.Types.Builder as BD
import qualified Development.Duplo.Types.Config as TC
import qualified Development.Duplo.Types.Options as OP

shakeMain :: String -> [String] -> TC.BuildConfig -> OP.Options -> IO ()
shakeMain cmdName cmdArgs config options = shake shakeOptions $ do
    let port       = config ^. TC.port
    let cwd        = config ^. TC.cwd
    let utilPath   = config ^. TC.utilPath
    let miscPath   = config ^. TC.miscPath
    let targetPath = config ^. TC.targetPath
    let bumpLevel  = config ^. TC.bumpLevel
    let appName    = config ^. TC.appName
    let appVersion = config ^. TC.appVersion
    let appId      = config ^. TC.appId
    let duploPath  = config ^. TC.duploPath

    -- What to build and each action's related action
    let targetScript = targetPath </> "index.js"
    let targetStyle  = targetPath </> "index.css"
    let targetMarkup = targetPath </> "index.html"
    targetScript *> (void . runExceptT . Scripts.build config)
    targetStyle  *> (void . runExceptT . Styles.build config)
    targetMarkup *> (void . runExceptT . Markups.build config)

    -- Manually bootstrap Shake
    action $ do
      -- Keep a list of commands so we can check before we call Shake,
      -- which doesn't allow us to change the error message when an action
      -- isn't found.
      let actions = [ "static"
                    , "clean"
                    , "build"
                    , "bump"
                    , "init"
                    , "version"
                    ]

      -- Default to help
      let cmdName' = if (cmdName `elem` actions) then cmdName else "help"
      -- Call command
      need [cmdName']

      -- Trailing space
      putNormal ""

    -- Handling static assets
    (Static.qualify config) &?> Static.build config

    "static" ~> Static.deps config

    "clean" ~> do
      -- Clean only when the target is there
      needCleaning <- doesDirectoryExist targetPath
      if   needCleaning
      then liftIO $ removeFiles targetPath ["//*"]
      else return ()

      logAction "Clean completed"

    "build" ~> do
      -- Always rebuild if we're building for production.
      if   TC.isInDev config
      then return ()
      else need ["clean"]

      -- Copy over static files first
      need ["static"]
      -- Then compile
      need [targetScript, targetStyle, targetMarkup]

      logAction "Build completed"

    "bump" ~> do
      (oldVersion, newVersion) <- Git.commit config bumpLevel

      logAction $ "Bumped version from " ++ oldVersion ++ " to " ++ newVersion

    "init" ~> do
      let user = cmdArgs ^. element 0
      let repo = cmdArgs ^. element 1
      let name = user ++ "/" ++ repo
      let src = miscPath </> "boilerplate/"
      let dest = cwd ++ "/"

      -- Check prerequisites
      when (length user == 0) $ throw BD.MissingGithubUserException
      when (length repo == 0) $ throw BD.MissingGithubRepoException

      logAction $ "Creating new duplo project " ++ name

      -- Initialize with boilerplate
      command_ [] (utilPath </> "init-boilerplate.sh") [src, dest]

      -- Update fields
      appInfo <- liftIO CM.readManifest
      let newAppInfo = appInfo { AI.name = repo
                               , AI.repo = name
                               }
      -- Commit app info
      liftIO $ CM.writeManifest newAppInfo

      -- Initalize git
      command_ [] (utilPath </> "init-git.sh") [name]

      logAction $ "Project created at " ++ dest

    -- Version should have already been displayed if requested
    "version" ~> return ()

    "help" ~> liftIO ((readFile $ miscPath </> "help.txt") >>= putStr)
