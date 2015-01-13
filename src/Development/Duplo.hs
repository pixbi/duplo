module Development.Duplo where

import           Control.Exception               (throw)
import           Control.Lens                    hiding (Action)
import           Control.Monad                   (unless, void, when)
import           Control.Monad.Trans.Maybe       (runMaybeT)
import qualified Development.Duplo.Component     as CM
import           Development.Duplo.Git           as Git
import           Development.Duplo.Markups       as Markups
import           Development.Duplo.Scripts       as Scripts
import           Development.Duplo.Static        as Static
import           Development.Duplo.Styles        as Styles
import qualified Development.Duplo.Types.AppInfo as AI
import qualified Development.Duplo.Types.Builder as BD
import qualified Development.Duplo.Types.Config  as TC
import qualified Development.Duplo.Types.Options as OP
import           Development.Duplo.Utilities     (createIntermediaryDirectories,
                                                  createStdEnv,
                                                  headerPrintSetter, logStatus,
                                                  successPrintSetter)
import           Development.Shake
import qualified Development.Shake               as DS
import           Development.Shake.FilePath      ((</>))
import           System.Console.GetOpt           (ArgDescr (..), OptDescr (..))
import           System.IO                       (readFile)

shakeOpts = shakeOptions { shakeThreads = 4 }

build :: String -> [String] -> TC.BuildConfig -> OP.Options -> IO ()
build cmdName cmdArgs config options = shake shakeOpts $ do
    let headerPrinter  = liftIO . logStatus headerPrintSetter
    let successPrinter = liftIO . logStatus successPrintSetter

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
    targetScript *> (void . runMaybeT . Scripts.build config)
    targetStyle  *> (void . runMaybeT . Styles.build config)
    targetMarkup *> (void . runMaybeT . Markups.build config)

    -- Manually bootstrap Shake
    action $ do
      -- Keep a list of commands so we can check before we call Shake,
      -- which doesn't allow us to change the error message when an action
      -- isn't found.
      let actions = [ "static"
                    , "clean"
                    , "build"
                    , "test"
                    , "bump"
                    , "init"
                    , "version"
                    ]

      -- Default to help
      let cmdName' = if cmdName `elem` actions then cmdName else "help"
      -- Call command
      need [cmdName']

      -- Trailing space
      putNormal ""

    -- Handling static assets
    Static.qualify config &?> Static.build config

    "static" ~> Static.deps config

    -- Install dependencies.
    "deps" ~> do
      liftIO $ logStatus headerPrintSetter "Installing dependencies"

      envOpt <- createStdEnv config

      command_ [envOpt] (utilPath </> "install-deps.sh") []

    "clean" ~> do
      -- Clean only when the target is there.
      needCleaning <- doesDirectoryExist targetPath
      when needCleaning $ liftIO $ removeFiles targetPath ["//*"]

      successPrinter "Clean completed"

    "build" ~> do
      -- Make sure all static files and dependencies are there.
      need ["static", "deps"]
      -- Then compile, in parallel.
      need [targetScript, targetStyle, targetMarkup]

      successPrinter "Build completed"

    "test" ~> do
      envOpt              <- createStdEnv config
      let appPath         =  config ^. TC.appPath
      let targetPath      =  config ^. TC.targetPath </> "tests"
      let utilPath        =  config ^. TC.utilPath
      let testPath        =  config ^. TC.testPath
      let testCompiler    =  utilPath </> "scripts-test.sh"
      let find path pttrn =  command [Cwd path] (utilPath </> "find.sh") [".", pttrn]

      -- There must be a test directory.
      testsExist <- doesDirectoryExist testPath
      unless testsExist $ throw BD.MissingTestDirectory

      -- Do a semi-full build
      need ["static", "deps"]
      need [targetScript, targetStyle]

      let prepareFile path =
            do
              let absPath = targetPath </> path
              -- Create intermediary directories
              createIntermediaryDirectories absPath
              -- Inject into each app file dependencies, AMD, etc in
              -- preparation for testing.
              Stdout compiled <- command [envOpt] testCompiler [path]
              -- Then write to the respective path in the output directory.
              writeFileChanged absPath compiled

      -- Each path is relative to the application root (most likely
      -- `app/`).
      Stdout codePaths' <- find testPath "*.js"
      mapM_ prepareFile $ lines codePaths'

      -- Build the markup once we have the script files in target.
      need [targetMarkup]

      -- Run the test suite
      command_ [envOpt] (utilPath </> "run-test.sh") []

      -- Copy over
      successPrinter "Tests completed"

    "bump" ~> do
      (oldVersion, newVersion) <- Git.commit config bumpLevel

      successPrinter $ "Bumped version from " ++ oldVersion ++ " to " ++ newVersion

    "init" ~> do
      let user = cmdArgs ^. element 0
      let repo = cmdArgs ^. element 1
      let name = user ++ "/" ++ repo
      let src  = miscPath </> "boilerplate/"
      let dest = cwd ++ "/"

      -- Check prerequisites
      when (null user) $ throw BD.MissingGithubUserException
      when (null repo) $ throw BD.MissingGithubRepoException

      headerPrinter $ "Creating new duplo project " ++ name

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

      successPrinter $ "Project created at " ++ dest

    -- Version should have already been displayed if requested
    "version" ~> return ()

    "help" ~> liftIO (readFile (miscPath </> "help.txt") >>= putStr)
