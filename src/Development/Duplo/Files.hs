{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.Files
  ( File(..)
  , readFile
  , filePath
  , fileDir
  , fileName
  , componentId
  , fileContent
  , isRoot
  , ComponentId
  ) where

import Prelude hiding (readFile)
import Development.Shake hiding (readFile)
import Data.Text (split, pack, unpack)
import Control.Lens hiding (Action)
import Control.Lens.TH (makeLenses)
import Data.List (intercalate)
import Development.Duplo.ComponentIO (appId)
import System.FilePath.Posix (makeRelative, splitDirectories, joinPath)
import Control.Monad.Trans.Class (lift)
import qualified Development.Duplo.ComponentIO as I

type FileName    = String
type FileContent = String
type ComponentId = String
data File        = File { _filePath    :: FilePath
                        , _fileDir     :: FilePath
                        , _fileName    :: FileName
                        , _componentId :: ComponentId
                        , _fileContent :: String
                        -- Is this part of the root project?
                        , _isRoot      :: Bool
                        } deriving (Show)

makeLenses ''File

readFile :: FilePath -> FilePath -> Action File
readFile cwd path = do
  let (fileDir, fileName) = parseFilePath path
  fileContent <- readFile' path
  appInfo <- liftIO I.readManifest
  let appId' = appId appInfo
  let componentId = parseComponentId cwd appId' fileDir
  let isRoot      = componentId == appId'
  return $ File path fileDir fileName componentId fileContent isRoot

parseFilePath :: FilePath -> (FilePath, FileName)
parseFilePath path =
  let
    segments  = splitDirectories path
    segLength = length segments
    dirLength = segLength - 1
    fileDir   = joinPath $ take dirLength segments
    fileName  = segments !! dirLength
  in
    (fileDir, fileName)

-- | Given a default component ID (usually the ID of the project on which
-- duplo is run) and the file path, deduce the component ID of a particular
-- file
parseComponentId :: FilePath -> ComponentId -> FilePath -> ComponentId
parseComponentId cwd defaultId path =
  let
    relPath  = makeRelative cwd path
    segments = splitDirectories relPath
  in
    case segments of
      ("components" : appId : xs) -> appId
      _                           -> defaultId
