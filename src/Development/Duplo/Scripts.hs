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
         , createIntermediaryDirectories
         , CompiledContent
         )
import Development.Shake
import Development.Shake.FilePath ((</>))
import Data.Text (replace, pack, unpack)
import Development.Duplo.Files (File(..), pseudoFile)
import qualified Development.Duplo.Config as C
import Control.Lens hiding (Action)
import Control.Monad.Trans.Class (lift)
import Development.Duplo.ComponentIO (extractCompVersions)
import qualified Language.JavaScript.Parser as JS
import Development.Duplo.JavaScript.Order (order)
import Control.Exception (throw)
import Development.Duplo.Types.JavaScript

      -- The environment
build :: C.BuildConfig
      -- The output file
      -> FilePath
      -- Doesn't need anything in return
      -> CompiledContent ()
build config = \ out -> do
  lift $ logAction "Building scripts"

  let cwd         = config ^. C.cwd
  let util        = config ^. C.utilPath
  let env         = config ^. C.env
  let input       = config ^. C.input
  let devPath     = config ^. C.devPath
  let devCodePath = devPath </> "modules/index.js"

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- These paths don't need to be expanded
  let staticPaths = [ "app/index.js"
                    ]

  -- These paths need to be expanded by Shake
  let dynamicPaths = [ "app/modules//*.js"
                     , "components/*/app/modules//*.js"
                     -- Compile dev files in dev mode as well.
                     ] ++ if   C.isInDev config
                          then ["dev/modules//*.js"]
                          else []

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

  -- Make sure we hvae at least something
  let duploIn' = if length duploIn > 0 then duploIn else "{}"

  -- Figure out each component's version
  compVers <- liftIO $ extractCompVersions cwd

  -- Inject global/environment variables
  let envVars = "var DUPLO_ENV = '" ++ env ++ "';\n"
             ++ "var DUPLO_IN = " ++ duploIn' ++ ";\n"
             ++ "var DUPLO_VERSIONS = " ++ compVers ++ ";\n"

  -- Configure the compiler
  let compiler = if   C.isInDev config
                 then util </> "scripts-dev.sh"
                 else util </> "scripts-optimize.sh"

  -- Create a pseudo file that contains the environment variables and
  -- prepend the file.
  let pre = return . (:) (pseudoFile { _fileContent = envVars })
  let post = return
           . either (throw . ParseException)
                    (JS.renderToString . order)
           . flip JS.parse ""

  -- Build it
  compiled <- compile config compiler [] paths pre post

  -- Write it to disk
  lift $ writeFileChanged out compiled
