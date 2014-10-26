module Development.Duplo.Utilities
  ( getDirectoryFilesInOrder
  , logAction
  , expandPaths
  , buildWith
  ) where

import Control.Monad (filterM)
import Data.List (intercalate)
import Development.Shake

type Params    = [String]
type Processor = [String] -> [String]

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

buildWith :: FilePath
          -> Params
          -> [FilePath]
          -> FilePath
          -> Processor
          -> Action ()
buildWith compiler params paths out process = do
  -- Log to console what we're dealing with
  mapM (putNormal . ("Including " ++)) paths

  -- Read 'em all
  contents <- mapM readFile' paths

  -- Pass to processor for specific manipulation
  let processed = process contents

  -- Trailing newline is significant in case of empty Stylus
  let concatenated = (intercalate "\n" processed) ++ "\n"

  -- Pass it through the compiler
  putNormal $ "Compiling with: " ++ compiler ++ " " ++ concat params
  Stdout compiled <- command [Stdin concatenated] compiler params

  -- Write output
  writeFileChanged out compiled
  putNormal ""

expandPaths :: String -> [String] -> [String] -> Action [String]
expandPaths cwd staticPaths dynamicPaths = do
  staticExpanded <- filterM doesFileExist (map (cwd ++) staticPaths)
  dynamicExpanded <- getDirectoryFilesInOrder cwd dynamicPaths
  return $ staticExpanded ++ dynamicExpanded
