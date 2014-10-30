{-# LANGUAGE ScopedTypeVariables #-}

module Development.Duplo.FileList
  ( File
  , CopyPair
  , makeFiles
  , handleFileList
  , toCopyPair
  ) where

import Development.Shake
import Data.Maybe (Maybe(..), fromMaybe, catMaybes)
import Control.Monad (msum)
import System.FilePath.Posix (makeRelative)
import Development.Shake.FilePath ((</>))

type File     = (FilePath, FilePath)
type CopyPair = (FilePath, FilePath)

makeFiles :: FilePath -> [FilePath] -> [File]
makeFiles base = fmap (makeFile base)

makeFile :: FilePath -> FilePath -> File
makeFile base path = (path, base)

handleFileList :: [File] -> Action (Maybe File)
handleFileList paths = do
    (paths' :: [Maybe File]) <- mapM handleFile paths
    return $ msum paths'

handleFile :: File -> Action (Maybe File)
handleFile file@(path, base) = do
    doesExist <- doesFileExist path

    if   doesExist
    then return $ Just file
    else return Nothing

toCopyPair :: FilePath -> File -> CopyPair
toCopyPair targetPath file@(path, base) =
    (from, to)
  where
    relPath = makeRelative base path
    from    = path
    to      = targetPath </> relPath
