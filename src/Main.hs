import PseudoMacros
{-import Development.Shake.Command-}
import Development.Shake.FilePath
{-import Development.Shake.Util-}
import Development.Shake
import Development.Duplo.Files
import Development.Duplo.Scripts
import Development.Duplo.ComponentIO (appRepo)
import System.Environment
import Control.Applicative ((<$>))
import Data.Maybe

main :: IO ()
main = do
  ----------
  -- Parameters

  -- TODO: `duploPath` should probably not have a default value
  duploPath <- fromMaybe "/" <$> lookupEnv "DUPLO_PATH"
  appMode   <- fromMaybe "dev" <$> lookupEnv "APP_MODE"
  appParams <- fromMaybe "{}" <$> lookupEnv "APP_PARAMS"

  let nodeModulesPath = combine duploPath "node_modules/.bin/"
  let runtimePath = combine duploPath "src/jsbits/runtime.js"

  shakeArgs shakeOptions $ do
    ----------
    -- Output paths

    let target     = "public/"
    let targetJs   = combine target "index.js"
    {-let targetCss  = combine target "index.css"-}
    {-let targetHtml = combine target "index.html"-}

    ----------
    -- Input paths

    let inputJsP   = getDirectoryFiles "" ["app/index.js", "app/modules//*.js", "components/*/app/modules//*.js"]
    {-let inputStylP = getDirectoryFiles "" ["app//*.styl", "components//*.styl"]-}
    -- Only the main Jade files in all components
    {-let inputJadeP = getDirectoryFiles "" ["app/index.jade", "components/*/app/index.jade"]-}

    ----------
    -- Compiler paths

    {-let stylCompiler = combine nodeModulesPath "stylus"-}
    {-let jadeCompiler = combine nodeModulesPath "jade"-}

    ----------
    -- Dependencies

    want [targetJs]
    {-want [targetJs, targetCss, targetHtml]-}

    ----------
    -- Do a build clean-up
    "clean" ~> do
      putNormal "Cleaning build files"
      cmd "rm" ["-r", "public/"]

    ----------
    -- Scripts
    targetJs *> \out -> do
      need [runtimePath]
      alwaysRerun

      -- The application repo is the default repo (when it's not a component
      -- repo)
      defaultRepo <- appRepo
      inputJs     <- inputJsP
      -- Collect all the applicable files
      files       <- mapM (expandFile defaultRepo) inputJs
      -- Duplo runtime
      runtime     <- readFile' runtimePath
      -- The app's settings
      let appSettings = "APP.mode = \"" ++ appMode ++ "\";\n"
                     ++ "APP.params = " ++ appParams ++ ";\n"
      -- Build the scripts
      let script = buildScript files appSettings runtime
      -- Commit the script file
      writeFileChanged out script

    {------------}
    {--- Stylesheet-}
    {-targetCss *> \out -> do-}
    {-  alwaysRerun-}
    {-  inputStyl    <- inputStylP-}
    {-  stylContents <- mapM readFile' inputStyl-}
    {-  -- Trailing newline is significant in case of empty Stylus-}
    {-  let styl = concat stylContents ++ "\n"-}
    {-  Stdout cssContents <- command [Stdin styl] stylCompiler []-}
    {-  writeFileChanged out cssContents-}

    {------------}
    {--- Markup-}
    {-targetHtml *> \out -> do-}
    {-  alwaysRerun-}
    {-  inputJade    <- inputJadeP-}
    {-  jadeContents <- mapM readFile' inputJade-}
    {-  -- Trailing newline is significant in case of empty Jade-}
    {-  let jade = concat jadeContents ++ "\n"-}
    {-  Stdout htmlContents <- command [Stdin jade] jadeCompiler []-}
    {-  writeFileChanged out htmlContents-}
