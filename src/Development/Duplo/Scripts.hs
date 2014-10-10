module Development.Duplo.Scripts (buildScript) where

import Data.List (intercalate)
import Development.Duplo.Files
import Control.Lens

-- | Build a script file given a list of files, a string to be placed at the
-- end.
buildScript :: [File] -> String -> String
buildScript files runtime = "(function () {\n"
                         ++ "var APP = {};\n"
                         -- All the modules
                         ++ meat ++ "\n"
                         -- Runtime must be at the end as it extracts modules
                         -- and removes `APP`
                         ++ runtime ++ "\n"
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
wrapModule modId content = "APP['" ++ modId ++ "'] = "
                        ++ "function (" ++ args ++ ") {\n"
                        ++ "var module = null;\n"
                        ++ "var exports = {};\n"
                        ++ content
                        ++ "return module.exports || exports;\n"
                        ++ "};\n"
  where
    -- Build module function's argument list
    args = intercalate ", " [ "MODE"
                            , "APP"
                            , "addEventListener"
                            , "removeEventListener"
                            , "dispatchEvent"
                            ]
