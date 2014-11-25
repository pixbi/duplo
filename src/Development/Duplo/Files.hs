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
  , pseudoFile
  ) where

import Control.Exception (throw)
import Control.Lens hiding (Action)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Data.Text (split, pack, unpack)
import Development.Duplo.Component (appId)
import Development.Shake hiding (readFile)
import Prelude hiding (readFile)
import System.FilePath.Posix (makeRelative, splitDirectories, joinPath)
import qualified Development.Duplo.Component as CM
import qualified Development.Duplo.Types.Builder as BD

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

pseudoFile = File { _filePath    = ""
                   , _fileDir     = ""
                   , _fileName    = ""
                   , _componentId = ""
                   , _fileContent = ""
                   , _isRoot      = False
                   }

makeLenses ''File

readFile :: FilePath -> FilePath -> ExceptT String Action File
readFile cwd path = do
  let (fileDir, fileName) = parseFilePath path
  fileContent <- lift $ readFile' path
  appInfo <- liftIO $ CM.readManifest
  let appId' = appId appInfo
  let componentId = parseComponentId cwd appId' fileDir
  let isRoot = componentId == appId'
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
