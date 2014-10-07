{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.Files (
  File, source, nodeModules,
  expandFile, getPath, getContent
  ) where

import PseudoMacros
import System.FilePath.Posix hiding (combine)
import Development.Shake.FilePath
import Control.Lens hiding (Action)
import Development.Shake
import Development.Duplo.ComponentIO (appRepo)

type File        = (FilePath, FileContent, [FileTag])
type FileTag     = String
type FileContent = String

source :: String
source = takeDirectory $__FILE__

nodeModules :: String
nodeModules = combine source "../../../node_modules/.bin/"

expandFile :: String -> Action File
expandFile path = do
  content <- readFile' path
  let tags = tagFilePath path
  return (path, content, tags)

getPath :: File -> String
getPath = view _1

getContent :: File -> String
getContent = view _2

tagFilePath :: String -> [FileTag]
tagFilePath path = appInfo
