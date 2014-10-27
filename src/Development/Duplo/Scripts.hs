module Development.Duplo.Scripts
  ( build
  ) where

import Control.Monad (filterM)
import Data.List (intercalate)
import Development.Duplo.Utilities
         ( getDirectoryFilesInOrder
         , logAction
         , expandPaths
         , buildWith
         )
import Development.Shake
import Development.Shake.FilePath (combine)
import Data.Text (replace, pack, unpack)
import Development.Duplo.Files (File(..))

build :: FilePath -> FilePath -> String -> String -> String -> FilePath -> Action ()
build cwd bin env mode input = \ out -> do
  logAction "Building scripts"

  -- These paths don't need to be expanded
  let staticPaths = [ "app/index.js"
                    ]

  -- These paths need to be expanded by Shake
  -- TODO: exclude dependencies not listed in the current mode
  let dynamicPaths = [ "app/modules//*.js"
                     , "components/*/app/modules//*.js"
                     ]

  -- Merge both types of paths
  paths <- expandPaths cwd staticPaths dynamicPaths

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

  -- Sanitize input
  let duploIn = unpack $
                  -- No newlines
                  replace (pack "\n") (pack "") $
                    -- Escape double-quotes
                    replace (pack "\"") (pack "\\\"") $
                      pack input
  -- Inject environment variables
  let envVars = "var DUPLO_ENV = DUPLO_ENV || \"" ++ env ++ "\";\n"
             ++ "var DUPLO_IN = DUPLO_IN || \"" ++ duploIn ++ "\";\n"

  -- Build it
  buildWith compiler params paths out $ \ files ->
    let
      -- Create a pseudo file that contains the environment variables
      envFile = File "" "" "" "" envVars
    in
      envFile : files
