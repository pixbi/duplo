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
import qualified Development.Duplo.FileList as FileList (filePath)
import Development.Shake.FilePath ((</>))

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

  -- Path to output index
  let targetIndex    = targetPath </> "index.html"
  -- Is it index in the target output directory?
  let isTargetIndex  = \ file -> targetIndex /= file ^. FileList.filePath
  -- Ignore index as it's handling by the markup action
  let filesLessIndex = filter isTargetIndex cleanedFiles

  -- We need to take the files and turn it into from/to pair
  let (froms, tos) = unzip $ toCopies targetPath filesLessIndex

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

-- | Build dependency list for static files
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
  devFiles'  <- getDirectoryFiles devPath ["//*"]
  let devFiles = if C.isInDev config then devFiles' else []
  -- Mix them together
  let allFiles = nub $ concat [assetFiles, devFiles]
  -- We do NOT want index
  let files    = filter ("index.html" /=) allFiles

  let getExt      = snd . splitExtension
  let getFilename = last . splitDirectories
  let firstFilenameChar = head . getFilename
  -- Only visible files (UNIX-style with leading dot)
  let onlyVisible = filter $ \ x -> '.' /= firstFilenameChar x
  let staticFiles = onlyVisible files
  -- Map to output equivalents
  let filesOut    = fmap (targetPath ++) staticFiles

  -- Declare dependencies
  need filesOut

qualify :: C.BuildConfig
        -> FilePath
        -> Maybe [FilePath]
qualify config path =
    if   targetPath ?== path
    then Just [path]
    else Nothing
  where
    targetPath = config ^. C.targetPath ++ "/*"
