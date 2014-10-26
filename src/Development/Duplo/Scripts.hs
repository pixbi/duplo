module Development.Duplo.Scripts (
    build
  ) where

import Control.Monad (filterM)
import Data.List (intercalate)
import Development.Duplo.Utilities (getDirectoryFilesInOrder, logAction)
import Development.Shake
import Development.Shake.FilePath (combine)
import Data.Text (replace, pack, unpack)

build :: FilePath -> FilePath -> String -> String -> String -> FilePath -> Action ()
build cwd bin env mode input = \ out -> do
  logAction "Building scripts"

  -- These paths don't need to be expanded
  let staticPaths' = [ "app/index.js"
                     ]
  staticPaths <- filterM doesFileExist staticPaths'

  -- These paths need to be expanded by Shake
  -- TODO: exclude dependencies not listed in the current mode
  let dynamicPaths' = [ "app/modules//*.js"
                      , "components/*/app/modules//*.js"
                      ]
  dynamicPaths <- getDirectoryFilesInOrder cwd dynamicPaths'

  -- Merge both types of paths
  let paths = staticPaths ++ dynamicPaths
  mapM (putNormal . ("Including " ++)) paths

  -- Inject environment variables
  let duploIn = unpack $ replace (pack "\n") (pack "") (pack input)
  let envVars = "var DUPLO_ENV = \"" ++ env ++ "\";\n"
             ++ "var DUPLO_IN = \"" ++ duploIn ++ "\";\n"

  -- Read 'em all
  contents <- mapM readFile' paths

  -- Trailing newline is significant in case of empty Stylus
  let concatenated = (intercalate "\n" contents) ++ "\n"

  -- Prepare Closure
  let closurePath = combine bin "compiler.jar"
  let closureParams = [ "-jar"
                      , closurePath
                      , "--compilation_level"
                      , "SIMPLE_OPTIMIZATIONS"
                      ]

  -- Pick the right compiler and parameters to build
  let compiler =
        case env of
          "dev" -> "bash"
          _     -> "java"
  let params =
        case env of
          "dev" -> [combine bin "echo.sh"]
          _     -> closureParams

  -- Pass it through the compiler
  putNormal $ "Compiling with: " ++ compiler ++ " " ++ concat params
  Stdout compiled <- command [Stdin concatenated] compiler params

  -- Write output
  writeFileChanged out compiled
  putNormal ""
