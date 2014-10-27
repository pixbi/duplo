module Development.Duplo.Utilities
  ( getDirectoryFilesInOrder
  , logAction
  , expandPaths
  , buildWith
  ) where

import Prelude hiding (readFile)
import Control.Monad (filterM)
import Data.List (intercalate)
import Development.Shake hiding (readFile)
import Development.Duplo.Files
         ( readFile
         , File(..)
         , filePath
         , fileDir
         , fileName
         , componentId
         , fileContent
         )
import Development.Shake.FilePath (combine)
import Control.Lens hiding (Action)

getDirectoryFilesInOrder :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFilesInOrder base patterns =
    -- Re-package the contents into the list of paths that we've wanted
    fmap concat files
  where
    -- We need to turn all elements into lists for each to be run independently
    patterns' = fmap (replicate 1) patterns
    -- Curry the function that gets the files given a list of paths
    getFiles  = getDirectoryFiles base
    -- Map over the list monadically to get the paths in order
    files     = mapM getFiles patterns'

logAction :: String -> Action ()
logAction log = do
  putNormal ""
  putNormal $ ">> " ++ log

-- | Given the path to a compiler, parameters to the compiler, a list of
-- paths of to-be-compiled files, the output file path, and a processing
-- function, do the following:
--
-- * reads all the to-be-compiled files
-- * calls the processor with the list of files to perform any
--   pre-processing
-- * concatenates all files
-- * passes the concatenated string to the compiler
-- * writes to the output file
          -- Current working directory
buildWith :: FilePath
          -- The path to the compiler
          -> FilePath
          -- The parameters passed to the compiler
          -> [String]
          -- Files to be compiled
          -> [FilePath]
          -- The output file
          -> FilePath
          -- The processing lambda
          -> ([File] -> [File])
          -- We don't return anything
          -> Action ()
buildWith cwd compiler params paths out process = do
  -- Log to console what we're dealing with
  mapM (putNormal . ("Including " ++)) paths

  -- Construct files
  files <- mapM (readFile cwd) paths

  -- Pass to processor for specific manipulation
  let processed = process files

  -- We only care about the content from this point on
  let contents = fmap (^. fileContent) processed

  -- Trailing newline is significant in case of empty Stylus
  let concatenated = (intercalate "\n" contents) ++ "\n"

  -- Pass it through the compiler
  putNormal $ "Compiling with: " ++ compiler ++ " " ++ intercalate " " params
  Stdout compiled <- command [Stdin concatenated] compiler params

  -- Write output
  writeFileChanged out compiled
  putNormal ""

expandPaths :: String -> [String] -> [String] -> Action [String]
expandPaths cwd staticPaths dynamicPaths = do
  let expand = map (combine cwd)
  staticExpanded <- filterM doesFileExist $ expand staticPaths
  dynamicExpanded <- getDirectoryFilesInOrder cwd dynamicPaths
  return $ staticExpanded ++ expand dynamicExpanded
