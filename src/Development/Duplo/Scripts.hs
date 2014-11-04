module Development.Duplo.Scripts
  ( build
  ) where

import Control.Monad (filterM)
import Data.List (intercalate)
import Development.Duplo.Utilities
         ( getDirectoryFilesInOrder
         , logAction
         , expandPaths
         , compile
         )
import Development.Shake
import Development.Shake.FilePath ((</>))
import Data.Text (replace, pack, unpack)
import Development.Duplo.Files (File(..))
import qualified Development.Duplo.Config as C
import Control.Lens hiding (Action)

      -- The environment
build :: C.BuildConfig
      -- The output file
      -> FilePath
      -- Doesn't need anything in return
      -> Action ()
build config = \ out -> do
  logAction "Building scripts"

  let cwd   = config ^. C.cwd
  let bin   = config ^. C.bin
  let env   = config ^. C.env
  let input = config ^. C.input

  -- These paths don't need to be expanded
  let staticPaths = [ "app/index.js"
                    ]

  -- These paths need to be expanded by Shake
  let dynamicPaths = [ "app/modules//*.js"
                     -- TODO: for now we hardwire bootloader in
                     , "components/pixbi-bootloader/app/*.js"
                     , "components/*/app/*.js"
                     , "components/*/app/modules//*.js"
                     ]

  -- Merge both types of paths
  paths <- expandPaths cwd staticPaths dynamicPaths

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

  -- Just pass through without compilation
  let compiler = bin </> "echo.sh"

  -- Build it
  compiled <- compile config compiler [] paths $ \ files ->
    -- Create a pseudo file that contains the environment variables
    let envFile = File { _fileContent = envVars }
    -- Prepend the environment variables
    in  envFile : files

  -- Write it to disk
  writeFileChanged out compiled
