module Development.Duplo.Static
  ( build
  , deps
  , qualify
  ) where

import Development.Duplo.Utilities (logAction, createIntermediaryDirectories, createPathDirectories)
import Development.Shake
import qualified Development.Duplo.Types.Config as TC
import Control.Lens hiding (Action)
import System.FilePath.Posix (splitExtension, splitDirectories, makeRelative)
import Data.List (transpose, nub)
import Control.Monad (zipWithM_, filterM)
import Development.Duplo.FileList (makeFile, makeFiles, toCopies, collapseFileLists, Copy)
import qualified Development.Duplo.FileList as FileList (filePath)
import Development.Shake.FilePath ((</>))

build :: TC.BuildConfig
      -> [FilePath]
      -> Action ()
build config = \ outs -> do
  let targetPath   = config ^. TC.targetPath
  let assetsPath   = config ^. TC.assetsPath
  let depsPath     = config ^. TC.depsPath
  let devPath      = config ^. TC.devPath
  let devAssetPath = devPath </> "assets"

  -- Convert to relative paths for copying
  let filesRel = fmap (makeRelative targetPath) outs
  -- Look in assets directory
  let assetFiles = makeFiles assetsPath filesRel
  -- Look in components' asset directories as well
  depAssetDirs <- getDirectoryDirs depsPath
  -- Assets are relative to their own asset directories
  let depAssetPaths = [ base </> dep </> src </> "assets"
                      | src  <- ["app"]
                      , base <- [depsPath]
                      , dep  <- depAssetDirs
                      ]
  -- Make the actual file records with the asset directory as the base
  let depAssetFiles = [ makeFile base file
                      | base <- depAssetPaths
                      , file <- filesRel
                      ]
  -- Look in the dev directory as well
  let devFiles = makeFiles devAssetPath filesRel

  -- Combine matching files into lists each pointing to its corresponding
  -- output file. Note that this is in order of precedence.
  let possibleFiles = transpose [devFiles, assetFiles, depAssetFiles]
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

  -- Make sure the intermediary directories exist
  mapM_ createIntermediaryDirectories tos

  -- Copy all files
  zipWithM_ copyFileChanged froms tos

-- | Build dependency list for static files
deps :: TC.BuildConfig
     -> Action ()
deps config = do
  logAction "Copying static files"

  let assetsPath = config ^. TC.assetsPath
  let depsPath   = config ^. TC.depsPath
  let targetPath = config ^. TC.targetPath
  let devPath    = config ^. TC.devPath
  let devAssetsPath = devPath </> "assets/"

  -- Make sure all these directories exist
  createPathDirectories [assetsPath, depsPath, targetPath, devAssetsPath]

  -- We want all asset files
  assetFiles    <- getDirectoryFiles assetsPath ["//*"]
  -- ... including those of dependencies
  depAssetFiles <- getDepAssets depsPath
  -- Add dev files to the mix, if we're in dev mode
  devFiles'     <- getDirectoryFiles devAssetsPath ["//*"]
  let devFiles   = if TC.isInDev config then devFiles' else []
  -- Mix them together
  let allFiles   = nub $ concat [depAssetFiles, assetFiles, devFiles]
  -- We do NOT want index
  let files      = filter ("index.html" /=) allFiles

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

qualify :: TC.BuildConfig
        -> FilePath
        -> Maybe [FilePath]
qualify config path =
    if   targetPath ?== path
    then Just [path]
    else Nothing
  where
    targetPath = config ^. TC.targetPath ++ "/*"

-- | Given where the dependencies are, return a list of assets relative to
-- their own containing components
getDepAssets :: FilePath -> Action [FilePath]
getDepAssets depsPath = do
    -- Get all dependencies' root directories.
    depNames         <- getDirectoryDirs depsPath
    -- In this context, everything is relative to the asset directory.
    let depAssetDirs  = fmap ((depsPath </>) . (</> "app/assets")) depNames
    -- See if there are even assets.
    applicableDeps   <- filterM doesDirectoryExist depAssetDirs

    -- Fetch each depednecy's asset files
    let assets  = [ getDirectoryFiles dep ["//*"]
                  | dep <- applicableDeps
                  ]

    -- Return the flatten version of all asset files
    fmap concat $ sequence assets
