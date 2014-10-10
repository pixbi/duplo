module Development.Duplo.Files (
  File, FileContent, ModuleId,
  expandFile, getFilePath, getFileContent, getModuleId
  ) where

import System.FilePath.Posix hiding (combine)
import Development.Shake.FilePath
import Control.Lens hiding (Action)
import Development.Shake
import Data.List.Split (splitOn)
import Data.List (intercalate)

type File        = (FilePath, FileContent, ModuleId)
type FileContent = String
type ModuleId    = String

-- e.g. pixbi/duplo
type Repo     = String
type Path     = [String]
type Location = (Repo, Path)

expandFile :: Repo -> FilePath -> Action File
expandFile appRepo path = do
  content <- readFile' path
  let modId = moduleId $ parsePath appRepo path
  return (path, content, modId)

getFilePath :: File -> String
getFilePath = view _1

getFileContent :: File -> String
getFileContent = view _2

getModuleId :: File -> String
getModuleId = view _3

moduleId :: Location -> ModuleId
moduleId (repo, loc) =
  let
    -- Drop filename if it's `index` as we assume that's the entry point to a
    -- particular module directory
    loc'     = if   loc ^. _last == "index"
               then loc ^. _init
               else loc
    -- Convert Component.IO-style name to dot notation
    repoPath = intercalate "." $ splitOn "-" repo
    -- Merge path segments
    locPath  = intercalate "." loc'
  in
    intercalate "." $ filter (not . null) [repoPath, locPath]

parsePath :: Repo -> FilePath -> Location
parsePath appRepo path =
  let
    -- The entire path without the file extension
    noExtPath = intercalate "." $ splitOn "." path ^. _init
    -- Split remaining path to segments split by filesystem separator
    segments  = splitOn "/" noExtPath
    -- App repo normalized to Component.IO convention
    appRepo'  = intercalate "-" $ splitOn "/" appRepo
  in
    case segments of
      -- Libraries
      ("components":repo:"app":"modules":loc) -> (repo, loc)
      -- Top-level application
      ("app":"modules":loc) -> (appRepo', loc)
      -- Special treatment for the app entry point
      ("app":"index":_) -> ("main", [])
      -- TODO: use MaybeT here
      _ -> ([], [])
      -- _ -> Nothing
