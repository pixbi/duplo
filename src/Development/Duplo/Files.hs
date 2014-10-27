module Development.Duplo.Files
  ( File(..)
  , readFile
  , getFilePath
  , getFileContent
  ) where

import Prelude hiding (readFile)
import Development.Shake hiding (readFile)
import Data.Text (split, pack, unpack)
{-import System.FilePath.Posix hiding (combine)-}
{-import Development.Shake.FilePath-}
{-import Control.Lens hiding (Action)-}
{-import Development.Shake-}
{-import Data.List.Split (splitOn)-}
import Data.List (intercalate)
import Development.Duplo.ComponentIO (appRepo)

type FileDir       = String
type FileName      = String
type FileContent   = String
type ComponentName = String
data File          = File FilePath FileDir FileName ComponentName FileContent

readFile :: FilePath -> Action File
readFile path = do
  let (fileDir, fileName) = parseFilePath path
  fileContent <- readFile' path
  return $ File path fileDir fileName "" fileContent

parseFilePath:: FilePath -> (FileDir, FileName)
parseFilePath path =
    (fileDir, fileName)
  where
    slash     = pack "/"
    path'     = pack path
    segments  = fmap unpack $ split (== '/') path'
    segLength = length segments
    dirLength = segLength - 1
    fileDir   = intercalate "/" $ take dirLength segments
    fileName  = segments !! dirLength

getFilePath :: File -> FilePath
getFilePath (File path _ _ _ _) = path

getFileContent :: File -> FilePath
getFileContent (File _ _ _ _ content) = content

getComponentId :: File -> String
getComponentId (File _ _ _ id _) = id
