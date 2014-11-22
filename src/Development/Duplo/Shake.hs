module Development.Duplo.Shake
  ( shakeMain
  ) where

import Development.Duplo.Utilities (logAction)
import Development.Shake
import Development.Shake.FilePath ((</>))
import qualified Development.Duplo.ComponentIO as I
import Control.Monad.Except (runExceptT)
import qualified Development.Duplo.Types.Config as C
import qualified Development.Duplo.Types.AppInfo as AI
import Control.Lens hiding (Action)
import Development.Duplo.Markups as Markups
import Development.Duplo.Styles as Styles
import Development.Duplo.Scripts as Scripts
import Development.Duplo.Static as Static
import Development.Duplo.Git as Git
import Control.Monad (void)

shakeMain :: String -> [String] -> C.BuildConfig -> IO ()
shakeMain cmd cmdArgs config = shake shakeOptions $ do
    let cwd = config ^. C.cwd
    let utilPath = config ^. C.utilPath
    let miscPath = config ^. C.miscPath
    let targetPath = config ^. C.targetPath
    let bumpLevel' = config ^. C.bumpLevel

    let bumpLevel = if   bumpLevel' `elem` ["patch", "minor", "major"]
                    then bumpLevel'
                    else "patch"

    -- What to build
    let targetScript = targetPath </> "index.js"
    let targetStyle  = targetPath </> "index.css"
    let targetMarkup = targetPath </> "index.html"

    targetMarkup *> (void . runExceptT . Markups.build config)
    targetStyle  *> (void . runExceptT . Styles.build config)
    targetScript *> (void . runExceptT . Scripts.build config)

    -- Manually bootstrap Shake
    action $ need [cmd]

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

    "version" ~> do
      -- Version information should already have
      return ()

    "build" ~> do
      -- Always rebuild if we're building for production.
      if   C.isInDev config
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
      let src  = miscPath </> "boilerplate/"
      let dest = cwd ++ "/"

      logAction $ "Creating new duplo project " ++ name

      -- Initialize with boilerplate
      command_ [] (utilPath </> "init-boilerplate.sh") [src, dest]

      -- Update fields
      Right appInfo <- liftIO $ runExceptT $ I.readManifest
      let newAppInfo = appInfo { AI.name = repo
                               , AI.repo = name
                               }
      -- Commit app info
      liftIO $ I.writeManifest newAppInfo

      -- Initalize git
      command_ [] (utilPath </> "init-git.sh") [name]

      logAction $ "Project created at " ++ dest
