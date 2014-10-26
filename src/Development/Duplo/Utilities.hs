module Development.Duplo.Utilities (
    getDirectoryFilesInOrder
  ) where

import Development.Shake

getDirectoryFilesInOrder :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFilesInOrder path patterns =
    -- Re-package the contents into the list of paths that we've wanted
    fmap concat files
  where
    -- We need to turn all elements into lists for each to be run independently
    patterns' = fmap (replicate 1) patterns
    -- Curry the function that gets the files given a list of paths
    getFiles  = getDirectoryFiles path
    -- Map over the list monadically to get the paths in order
    files     = mapM getFiles patterns'
