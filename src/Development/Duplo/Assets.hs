module Development.Duplo.Assets
  ( build
  , qualify
  ) where

{-import Control.Monad (filterM)-}
{-import Data.List (intercalate)-}
{-import Development.Duplo.Utilities-}
{-         ( getDirectoryFilesInOrder-}
{-         , logAction-}
{-         , expandPaths-}
{-         , buildWith-}
{-         )-}
import Development.Shake
{-import Development.Shake.FilePath ((</>))-}
{-import Data.Text (replace, pack, unpack)-}
{-import Development.Duplo.Files (File(..))-}
import qualified Development.Duplo.Config as C
import Control.Lens hiding (Action)
import System.FilePath.Posix (splitExtension)

      -- The environment
build :: C.BuildConfig
      -- The output file
      -> FilePath
      -- Doesn't need anything in return
      -> Action ()
build = undefined
{-build config = \ out -> do-}
{-  logAction "Building scripts"-}

{-  let cwd   = config ^. C.cwd-}
{-  let bin   = config ^. C.bin-}
{-  let env   = config ^. C.env-}
{-  let input = config ^. C.input-}

{-  -- These paths don't need to be expanded-}
{-  let staticPaths = [ "app/index.js"-}
{-                    ]-}

{-  -- These paths need to be expanded by Shake-}
{-  -- TODO: exclude dependencies not listed in the current mode-}
{-  let dynamicPaths = [ "app/modules//*.js"-}
{-                     , "components/*/app/modules//*.js"-}
{-                     ]-}

{-  -- Merge both types of paths-}
{-  paths <- expandPaths cwd staticPaths dynamicPaths-}

{-  -- Sanitize input-}
{-  let duploIn = unpack $-}
{-                  -- No newlines-}
{-                  replace (pack "\n") (pack "") $-}
{-                    -- Escape double-quotes-}
{-                    replace (pack "\"") (pack "\\\"") $-}
{-                      pack input-}

{-  -- Inject environment variables-}
{-  let envVars = "var DUPLO_ENV = DUPLO_ENV || \"" ++ env ++ "\";\n"-}
{-             ++ "var DUPLO_IN = DUPLO_IN || \"" ++ duploIn ++ "\";\n"-}

{-  -- Just pass through without compilation-}
{-  let compiler = "bash"-}
{-  let params   = [bin </> "echo.sh"]-}

{-  -- Build it-}
{-  buildWith cwd compiler params paths out $ \ files ->-}
{-    -- Create a pseudo file that contains the environment variables-}
{-    let envFile = File { _fileContent = envVars }-}
{-    -- Prepend the environment variables-}
{-    in  envFile : files-}

qualify :: C.BuildConfig
        -> FilePath
        -> Maybe [FilePath]
qualify config path =
    -- Anything in target directory other than the usual JS/CSS/HTML
    if   targetPath ?== path && not (extension `elem` exclude)
    then Just [path]
    else Nothing
  where
    targetPath = config ^. C.targetPath ++ "/*"
    exclude    = [".js", ".css", ".html"]
    extension  = (snd . splitExtension) path
