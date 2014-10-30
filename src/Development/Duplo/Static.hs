module Development.Duplo.Static
  ( build
  , deps
  , qualify
  ) where

import Development.Duplo.Utilities (logAction)
import Development.Shake
import qualified Development.Duplo.Config as C
import Control.Lens hiding (Action)
import System.FilePath.Posix (splitExtension, splitDirectories, makeRelative)
import Data.List (transpose, nub)
import Control.Monad (zipWithM_)
import Development.Duplo.FileList (makeFiles, toCopies, collapseFileLists, Copy)

build :: C.BuildConfig
      -> [FilePath]
      -> Action ()
build config = \ outs -> do
  let targetPath = config ^. C.targetPath
  let assetsPath = config ^. C.assetsPath
  let devPath    = config ^. C.devPath

  -- Convert to relative paths for copying
  let filesRel = fmap (makeRelative targetPath) outs
  -- Look in assets directory
  let assetFiles = makeFiles assetsPath filesRel
  -- Look in the dev directory as well
  let devFiles = makeFiles devPath filesRel

  -- Combine matching files into lists each pointing to its corresponding
  -- output file. Note that `devFiles` are *first*.
  let possibleFiles = transpose [devFiles, assetFiles]
  -- Each file list collapses into a path that exists
  cleanedFiles <- collapseFileLists possibleFiles
  -- We need to take the files and turn it into from/to pair
  let (froms, tos) = unzip $ toCopies targetPath cleanedFiles

  -- Log
  let repeat'  = replicate $ length froms
  let messages = transpose [ repeat' "Copying "
                           , froms
                           , repeat' " to "
                           , tos
                           ]
  mapM_ (putNormal . concat) messages

  -- Copy all files
  zipWithM_ copyFileChanged froms tos

-- Build dependency list for static files
deps :: C.BuildConfig
     -> Action ()
deps config = do
  logAction "Copying static files"

  let assetsPath = config ^. C.assetsPath
  let targetPath = config ^. C.targetPath
  let devPath    = config ^. C.devPath

  -- We want all asset files
  assetFiles <- getDirectoryFiles assetsPath ["//*"]
  -- Add dev files to the mix, if we're in dev mode
  devFiles' <- getDirectoryFiles devPath ["//*"]
  let devFiles = if C.isInDev config then devFiles' else []
  -- Mix them together
  let files = nub $ concat [assetFiles, devFiles]

  -- Anything other than the usual JS/CSS/HTML
  let exclude     = [".js", ".css", ".html"]
  let isCode      = flip elem exclude
  let getExt      = snd . splitExtension
  let getFilename = last . splitDirectories
  let onlyNonCode = filter $ not . isCode . getExt
  let firstFilenameChar = head . getFilename
  -- Only visible files (UNIX-style with leading dot)
  let onlyVisible = filter $ \ x -> '.' /= firstFilenameChar x
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
