{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.FileList where

import Control.Lens hiding (Action)
import Control.Lens.TH (makeLenses)
import Control.Monad (liftM, msum)
import Data.Maybe (Maybe(..), catMaybes)
import Development.Shake
import Development.Shake.FilePath ((</>))
import System.FilePath.Posix (makeRelative)

data File = File { _filePath :: FilePath
                 , _fileBase :: FilePath
                 } deriving (Show)
type Copy = (FilePath, FilePath)

defaultFile :: File
defaultFile = File { _filePath = ""
                   , _fileBase = ""
                   }

makeLenses ''File

-- | Given a base and a list of relative paths, transform into file objects
makeFiles :: FilePath -> [FilePath] -> [File]
makeFiles = fmap . makeFile

-- | Given a base and a relative path, return a file record containing the
-- absolute path with the base
makeFile :: FilePath -> FilePath -> File
makeFile base relPath =
    setPath absPath
  where
    constructor = defaultFile { _fileBase = base }
    absPath     = base </> relPath
    setPath     = (constructor &) . (filePath .~)

-- | Collapse lists of possible files and return the first file that exists
-- for each list
collapseFileLists :: [[File]] -> Action [File]
collapseFileLists = liftM catMaybes . mapM collapseFileList

-- | Given a list of possible files, reduce to a file that exists, or
-- nothing
collapseFileList :: [File] -> Action (Maybe File)
collapseFileList = liftM msum . mapM collapseFile

-- | Given a file, return itself if it exists
collapseFile :: File -> Action (Maybe File)
collapseFile file = do
    let path = file ^. filePath
    doesExist <- doesFileExist path

    if   doesExist
    then return $ Just file
    else return Nothing

-- | Given an output path and a list of file objects, convert all files
-- objects to copy pairs. See `toCopy` for more information.
toCopies :: FilePath -> [File] -> [Copy]
toCopies base = fmap $ toCopy base

-- | Given an output path and a file object, return a copy pair whose to
-- path is relative to the output path in the same way as the provided file
-- object's path is relative to its base.
toCopy :: FilePath -> File -> Copy
toCopy targetBase file =
    (from, to)
  where
    path    = file ^. filePath
    base    = file ^. fileBase
    relPath = makeRelative base path
    from    = path
    to      = targetBase </> relPath
