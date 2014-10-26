import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Development.Duplo.Styles as Styles
import Development.Shake
import Development.Shake.FilePath (combine)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Environment.Executable (splitExecutablePath)
{-import Data.String.Utils-}
{-import Development.Duplo.ComponentIO (appRepo)-}
{-import Development.Duplo.Files-}
{-import Development.Duplo.Markup as Files-}
{-import Development.Duplo.Scripts as Scripts-}
{-import Development.Shake.Command-}
{-import Development.Shake.Util-}

main :: IO ()
main = do
  -- Environment - e.g. dev, staging, live
  duploEnv <- fromMaybe "dev" <$> lookupEnv "DUPLO_ENV"
  -- Build mode, for dependency selection
  duploMode <- fromMaybe "" <$> lookupEnv "DUPLO_MODE"
  -- Current directory
  cwd <- getCurrentDirectory
  -- Duplo directory
  duploExecPath <- splitExecutablePath
  let (duploExecDir, _) = duploExecPath
  -- Application parameter
  duploIn <- getContents

  let duploDir        = combine duploExecDir "../.."
  let nodeModulesPath = combine duploDir "node_modules/.bin"
  let utilPath        = combine duploDir "util"

  let target        = combine cwd "public/"
  let targetScripts = combine target "index.js"
  let targetStyles  = combine target "index.css"
  let targetMarkups = combine target "index.html"

  shakeArgs shakeOptions $ do
    want [targetStyles]
    {-want [targetScripts, targetStyles, targetMarkups]-}

    {-targetScripts *> Scripts.build cwd utilPath-}
    targetStyles *> Styles.build cwd nodeModulesPath
    {-targetMarkups *> Markups.build cwd nodeModulesPath-}

    "clean" ~> do
      putNormal "Cleaning build files"
      cmd "rm" ["-r", "public/"]

    "help" ~> do
      cmd "cat" [combine duploDir "etc/help.txt"]
