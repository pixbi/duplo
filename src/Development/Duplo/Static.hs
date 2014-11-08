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
import Control.Monad (zipWithM_, filterM)
import Development.Duplo.FileList
         ( makeFile
         , makeFiles
         , toCopies
         , collapseFileLists
         , Copy
         )
import qualified Development.Duplo.FileList as FileList (filePath)
import Development.Shake.FilePath ((</>))
import System.FilePath.Posix (joinPath, splitPath)

build :: C.BuildConfig
      -> [FilePath]
      -> Action ()
build config = \ outs -> do
  let targetPath = config ^. C.targetPath
  let assetsPath = config ^. C.assetsPath
  let depsPath   = config ^. C.depsPath
  let devPath    = config ^. C.devPath

  -- Convert to relative paths for copying
  let filesRel = fmap (makeRelative targetPath) outs
  -- Look in assets directory
  let assetFiles = makeFiles assetsPath filesRel
  -- Look in components' asset directories as well
  depAssetDirs <- getDirectoryDirs depsPath
  -- Assets are relative to their own asset directories
  let depAssetPaths = [ base </> dep </> "app/assets"
                      | base <- [depsPath]
                      , dep  <- depAssetDirs
                      ]
  -- Make the actual file records with the asset directory as the base
  let depAssetFiles = [ makeFile base file
                      | base <- depAssetPaths
                      , file <- filesRel
                      ]
  -- Look in the dev directory as well
  let devFiles = makeFiles devPath filesRel

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

-- | This should be self-evident.
createIntermediaryDirectories :: String -> Action ()
createIntermediaryDirectories path =
    command_ [] "mkdir" ["-p", dir]
  where
    dir = joinPath $ init $ splitPath path

-- | Build dependency list for static files
deps :: C.BuildConfig
     -> Action ()
deps config = do
  logAction "Copying static files"

  let assetsPath = config ^. C.assetsPath
  let depsPath   = config ^. C.depsPath
  let targetPath = config ^. C.targetPath
  let devPath    = config ^. C.devPath

  -- Make sure all these directories exist
  let requiredPaths = [assetsPath, depsPath, targetPath, devPath]
  let mkdir = \ dir -> command_ [] "mkdir" ["-p", dir]
  existing <- filterM ((fmap not) . doesDirectoryExist) requiredPaths
  mapM_ mkdir existing

  -- We want all asset files
  assetFiles    <- getDirectoryFiles assetsPath ["//*"]
  -- ... including those of dependencies
  depAssetFiles <- getDepAssets depsPath
  -- Add dev files to the mix, if we're in dev mode
  devFiles'     <- getDirectoryFiles devPath ["//*"]
  let devFiles   = if C.isInDev config then devFiles' else []
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

qualify :: C.BuildConfig
        -> FilePath
        -> Maybe [FilePath]
qualify config path =
    if   targetPath ?== path
    then Just [path]
    else Nothing
  where
    targetPath = config ^. C.targetPath ++ "/*"

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
