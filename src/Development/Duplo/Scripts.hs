module Development.Duplo.Scripts (buildScript) where

import Data.List (intercalate)
import Development.Duplo.Files
import Control.Lens

-- | Build a script file given a list of files, a string to be placed at the
-- the beginning and at the end.
buildScript :: [File] -> String -> String -> String
buildScript files prefix suffix
    = "(function () {\n"
   ++ "var APP = {};\n"
   ++ prefix ++ "\n"
   ++ meat ++ "\n"
   ++ suffix ++ "\n"
   ++ "})();\n"
  where
    scripts = wrapFiles files
    meat    = concat $ map getFileContent scripts

wrapFiles :: [File] -> [File]
wrapFiles = map wrapFile

wrapFile :: File -> File
wrapFile file = over _2 wrap file
  where
    modId = getModuleId file
    wrap  = wrapModule modId

wrapModule :: ModuleId -> FileContent -> FileContent
wrapModule modId content
    = "APP['" ++ modId ++ "'] = "
   ++ "function (" ++ args ++ ") {\n"
   ++ "var module = null;\n"
   ++ "var exports = {};\n"
   ++ content ++ "\n"
   ++ "return module.exports || exports;\n"
   ++ "};\n"
  where
    -- Build module function's argument list
    args = intercalate ", "
      [ "MODE"
      , "APP"
      , "addEventListener"
      , "removeEventListener"
      , "dispatchEvent"
      ]
