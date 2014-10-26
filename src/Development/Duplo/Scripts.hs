module Development.Duplo.Scripts (
    build
  ) where

build :: FilePath -> FilePath -> String -> String -> String -> FilePath -> Action a
build cwd bin env mode input out = \ out -> do
  alwaysRerun
  -- The application repo is the default repo (when it's not a component
  -- repo)
  defaultRepo <- appRepo
  inputJs     <- inputJsPaths
  -- Collect all the applicable files
  files       <- mapM (expandFile defaultRepo) inputJs
  -- The app's settings
  let appParams   = replace "\n" "" input
  let appSettings = "var DUPLO_ENV = \"" ++ env ++ "\";\n"
                 ++ "var DUPLO_IN = \"" ++ appParams ++ "\";\n"

  -- Build the scripts
  let script = concat $ map getFileContent files

  -- Prepare Closure
  let closurePath = combine bin "compiler.jar"
  let closureParams = [ "-jar"
                      , closurePath
                      , "--compilation_level"
                      , "SIMPLE_OPTIMIZATIONS"
                      ]
  -- Pick the right compiler and parameters to build
  let compiler =
        case mode of
          "live" -> "java"
          _      -> "bash"
  let params =
        case mode of
          "live" -> closureParams
          _      -> [combine bin "echo.sh"]

  -- Pass it through the compiler
  Stdout compiled <- command [Stdin script] compiler params

  -- Write output
  writeFileChanged out compiled
