import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Development.Duplo.Styles as Styles
import Development.Duplo.Utilities (logAction)
import Development.Shake
import Development.Shake.FilePath (combine)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Environment.Executable (splitExecutablePath)
{-import Data.String.Utils-}
{-import Development.Duplo.ComponentIO (appRepo)-}
{-import Development.Duplo.Files-}
{-import Development.Duplo.Markup as Files-}
import Development.Duplo.Scripts as Scripts
{-import Development.Shake.Command-}
{-import Development.Shake.Util-}

main :: IO ()
main = do
  -- Environment - e.g. dev, staging, live
  duploEnv <- fromMaybe "dev" <$> lookupEnv "DUPLO_ENV"
  -- Build mode, for dependency selection
  duploMode <- fromMaybe "" <$> lookupEnv "DUPLO_MODE"
  -- Application parameter
  duploIn <- fromMaybe "" <$> lookupEnv "DUPLO_IN"
  -- Current directory
  cwd <- getCurrentDirectory
  -- Duplo directory
  duploExecPath <- splitExecutablePath
  let (duploExecDir, _) = duploExecPath

  -- Paths to various relevant directories
  let duploDir        = combine duploExecDir "../.."
  let nodeModulesPath = combine duploDir "node_modules/.bin"
  let utilPath        = combine duploDir "util"

  -- What to build
  let target        = combine cwd "public/"
  let targetScripts = combine target "index.js"
  let targetStyles  = combine target "index.css"
  let targetMarkups = combine target "index.html"

  -- Report back what's given for confirmation
  putStr $ ">> Parameters \n"
        ++ "Current working directory:         " ++ cwd ++ "\n"
        ++ "duplo is installed at:             " ++ duploExecDir ++ "\n"
        ++ "Environment (i.e. `DUPLO_ENV`):    " ++ duploEnv ++ "\n"
        ++ "Build Mode (i.e. `DUPLO_MODE`):    " ++ duploMode ++ "\n"
        ++ "App Parameters (i.e. `DUPLO_IN`):  " ++ duploIn ++ "\n"
        ++ "\n"

  shakeArgs shakeOptions $ do
    -- Dependencies
    want [targetStyles, targetScripts]
    {-want [targetScripts, targetStyles, targetMarkups]-}

    -- Actions
    targetScripts *> Scripts.build cwd utilPath duploEnv duploMode duploIn
    targetStyles *> Styles.build cwd nodeModulesPath
    {-targetMarkups *> Markups.build cwd nodeModulesPath-}

    "clean" ~> do
      logAction "Cleaning built files"
      cmd "rm" ["-r", "public/"]

    "help" ~> do
      cmd "cat" [combine duploDir "etc/help.txt"]
