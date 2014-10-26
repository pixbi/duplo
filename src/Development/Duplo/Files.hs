module Development.Duplo.Files
  ( File(..)
  , readFile
  , getFilePath
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

type FileDir     = String
type FileName    = String
type FileContent = String
data File        = File FilePath FileDir FileName FileContent

readFile :: FilePath -> Action File
readFile path = do
  let (fileDir, fileName) = parseFilePath path
  fileContent <- readFile' path
  return $ File path fileDir fileName fileContent

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
getFilePath (File path _ _ _) = path
