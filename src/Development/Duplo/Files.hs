{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.Files where

import           Control.Lens                hiding (Action)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import           Development.Duplo.Component (appId)
import qualified Development.Duplo.Component as CM
import           Development.Shake           (Action, liftIO, readFile')
import           System.FilePath.Posix       (joinPath, makeRelative,
                                              splitDirectories)

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

pseudoFile :: File
pseudoFile = File { _filePath    = ""
                  , _fileDir     = ""
                  , _fileName    = ""
                  , _componentId = ""
                  , _fileContent = ""
                  , _isRoot      = False
                  }

makeLenses ''File

readFile :: FilePath -> FilePath -> MaybeT Action File
readFile cwd path = do
  let (fDir, fName) = parseFilePath path
  fContent <- lift $ readFile' path
  appInfo  <- liftIO CM.readManifest
  let appId'  = appId appInfo
  let cId     = parseComponentId cwd appId' fDir
  let isRoot' = cId == appId'
  return $ File path fDir fName cId fContent isRoot'

parseFilePath :: FilePath -> (FilePath, FileName)
parseFilePath path = (fDir, fName)
  where
    segments  = splitDirectories path
    segLength = length segments
    dirLength = segLength - 1
    fDir      = joinPath $ take dirLength segments
    fName     = segments !! dirLength

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
      ("components" : aId : _) -> aId
      _                        -> defaultId
