module Development.Duplo.Static
  ( build
  , qualify
  ) where

{-import Control.Monad (filterM)-}
{-import Data.List (intercalate)-}
import Development.Duplo.Utilities (logAction)
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
import System.FilePath.Posix (splitExtension, splitDirectories)

-- Build dependency list for static files
build :: C.BuildConfig
      -> Action ()
build config = do
  logAction "Copying static files"

  -- We want all asset files
  let assetsPath = config ^. C.assetsPath
  let targetPath = config ^. C.targetPath
  files <- getDirectoryFiles assetsPath ["//*"]

  -- Anything other than the usual JS/CSS/HTML
  let exclude     = [".js", ".css", ".html"]
  let isCode      = flip elem exclude
  let getExt      = snd . splitExtension
  let getFilename = last . splitDirectories
  let onlyNonCode = filter $ not . isCode . getExt
  -- Only visible files (UNIX-style with leading dot)
  let onlyVisible = filter $ \ x ->
                      '.' /= (head . getFilename) x
  let staticFiles = onlyNonCode $ onlyVisible files
  -- Map to output equivalents
  let filesOut = fmap (targetPath ++) staticFiles

  -- Declare dependencies
  need filesOut

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
