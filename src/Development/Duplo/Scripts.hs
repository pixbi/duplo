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
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Development.Duplo.ComponentIO (extractCompVersions)

      -- The environment
build :: C.BuildConfig
      -- The output file
      -> FilePath
      -- Doesn't need anything in return
      -> MaybeT Action ()
build config = \ out -> do
  lift $ logAction "Building scripts"

  let cwd   = config ^. C.cwd
  let bin   = config ^. C.bin
  let env   = config ^. C.env
  let input = config ^. C.input

  -- These paths don't need to be expanded
  let staticPaths = [ "app/index.js"
                    ]

  -- These paths need to be expanded by Shake
  let dynamicPaths = [ "app/modules//*.js"
                     , "components/*/app/*.js"
                     , "components/*/app/modules//*.js"
                     ]

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd staticPaths dynamicPaths

  -- Sanitize input
  let duploIn = unpack $
                  -- No newlines
                  replace (pack "\n") (pack "") $
                    -- Single-quotes
                    replace (pack "'") (pack "\\'") $
                      -- Double-quotes
                      replace (pack "\\") (pack "\\\\") $
                        pack input

  -- Figure out each component's version
  compVers <- liftIO $ extractCompVersions cwd

  -- Inject global/environment variables
  let envVars = "var DUPLO_ENV = \"" ++ env ++ "\";\n"
             ++ "var DUPLO_IN = JSON.parse('" ++ duploIn ++ "' || '{}');\n"
             ++ "var DUPLO_VERSIONS = JSON.parse('" ++ compVers ++ "');\n"

  -- Just pass through without compilation
  let compiler = bin </> "echo.sh"

  -- Build it
  compiled <- compile config compiler [] paths $ \ files ->
    -- Create a pseudo file that contains the environment variables
    let envFile = File { _fileContent = envVars }
    -- Prepend the environment variables
    in  envFile : files

  -- Write it to disk
  lift $ writeFileChanged out compiled
