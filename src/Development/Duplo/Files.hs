module Development.Duplo.Files (
    File, FileDir, FileName, FileContent,
    getFilePath, getFileDir, getFileName, getFileContent,
    expandFile
  ) where

{-import System.FilePath.Posix hiding (combine)-}
{-import Development.Shake.FilePath-}
import Control.Lens hiding (Action)
{-import Development.Shake-}
{-import Data.List.Split (splitOn)-}
{-import Data.List (intercalate)-}

type FileDir     = String
type FileName    = String
type FileContent = String
type File        = (FilePath, FileDir, FileName, FileContent)

getFilePath    :: File -> String
getFilePath    = view _1
getFileDir     :: File -> String
getFileDir     = view _2
getFileName    :: File -> String
getFileName    = view _3
getFileContent :: File -> String
getFileContent = view _4

expandFile :: FilePath -> FileDir -> Action File
expandFile appRepo path = do
  content <- readFile' path
  let modId = moduleId $ parsePath appRepo path
  return (path, content, modId)
