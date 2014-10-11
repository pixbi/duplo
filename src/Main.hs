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
import Data.List (intercalate)

main :: IO ()
main = do
  ----------
  -- Parameters

  -- TODO: `duploPath` should probably not have a default value
  duploPath  <- fromMaybe "/" <$> lookupEnv "DUPLO_PATH"
  -- TODO: These default values are useless given that we're calling into this
  -- from bash
  appMode    <- fromMaybe "dev" <$> lookupEnv "APP_MODE"
  appParams' <- fromMaybe "{}" <$> lookupEnv "APP_PARAMS"

  -- App parameters need to be JSON-compatible
  let appParams =
        case appParams' of
          "" -> "{}"
          _  -> appParams'
  let nodeModulesPath = combine duploPath "node_modules/.bin/"
  let runtimePath = combine duploPath "src/jsbits/runtime.js"

  ----------
  -- Output paths

  let target     = "public/"
  let targetJs   = combine target "index.js"
  let targetCss  = combine target "index.css"
  {-let targetHtml = combine target "index.html"-}

  ----------
  -- Input paths

  -- Scripts
  let inputJsP = getDirectoryFiles "" [ "app/index.js"
                                      , "app/modules//*.js"
                                      , "components/*/app/modules//*.js"
                                      ]

  -- Styles
  let stylBases = [ "app/"
                  , "components/*/app/"
                  ]
  let stylPaths = [ "styl/variables.styl"
                  , "styl/keyframes.styl"
                  , "styl/fonts.styl"
                  , "styl/reset.styl"
                  , "styl/main.styl"
                  , "modules/**/index.styl"
                  ]
  let stylFullPaths = [ base ++ path | base <- stylBases, path <- stylPaths ]

  -- Only the main Jade files in all components
  {-let inputJadeP = getDirectoryFiles "" ["app/index.jade", "components/*/app/index.jade"]-}

  ----------
  -- Compiler paths

  let stylCompiler = combine nodeModulesPath "stylus"
  {-let jadeCompiler = combine nodeModulesPath "jade"-}

  shakeArgs shakeOptions $ do
    ----------
    -- Dependencies

    want [targetJs, targetCss]
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
      let appSettings = "APP._mode = \"" ++ appMode ++ "\";\n"
                     ++ "APP._params = " ++ appParams ++ ";\n"

      -- Build the scripts
      let script = buildScript files appSettings runtime

      -- Prepare Closure
      let closurePath = combine duploPath "util/compiler.jar"
      let closureParams = [ "-jar"
                          , closurePath
                          , "--compilation_level"
                          , "SIMPLE_OPTIMIZATIONS"
                          ]
      -- Pick the right compiler and parameters to build
      let compiler =
            case appMode of
              "live" -> "java"
              _      -> "bash"
      let params =
            case appMode of
              "live" -> closureParams
              _      -> [combine duploPath "util/echo.sh"]

      -- Pass it through the compiler
      Stdout compiled <- command [Stdin script] compiler params
      writeFileChanged out compiled

    ----------
    -- Stylesheet
    targetCss *> \out -> do
      let compiler = combine nodeModulesPath "stylus"
      alwaysRerun

      stylFiles <- getDirectoryFilesInOrder "" stylFullPaths
      stylContents <- mapM readFile' stylFiles
      -- Trailing newline is significant in case of empty Stylus
      let styl = (intercalate "\n" stylContents) ++ "\n"

      -- Pass it through the compiler
      Stdout compiled <- command [Stdin styl] compiler []
      writeFileChanged out compiled

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

getDirectoryFilesInOrder :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFilesInOrder path patterns =
    -- Re-package the contents into the list of paths that we've wanted
    fmap concat files
  where
    -- We need to turn all elements into lists for each to be run independently
    patterns' = fmap (replicate 1) patterns
    -- Curry the function that gets the files given a list of paths
    getFiles  = getDirectoryFiles path
    -- Map over the list monadically to get the paths in order
    files     = mapM getFiles patterns'
