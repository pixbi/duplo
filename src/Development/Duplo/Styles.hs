module Development.Duplo.Styles (
    build
  ) where

import Data.List (intercalate)
import Development.Duplo.Utilities (getDirectoryFilesInOrder)
import Development.Shake
import Development.Shake.FilePath (combine)

build :: FilePath -> FilePath -> FilePath -> Action ()
build cwd bin = \ out -> do
  -- TODO: to be removed after development
  alwaysRerun

  -- Paths
  let bases = [ "app/"
              , "components/*/app/"
              ]
  let paths' = [ "styl/variables.styl"
               , "styl/keyframes.styl"
               , "styl/fonts.styl"
               , "styl/reset.styl"
               , "styl/main.styl"
               , "modules/**/index.styl"
               ]
  paths <- getDirectoryFilesInOrder cwd
             [ base ++ path
             | base <- bases
             , path <- paths'
             ]

  -- Path to the compiler
  let compiler = combine bin "stylus"

  -- Read 'em all
  contents <- mapM readFile' paths

  -- Trailing newline is significant in case of empty Stylus
  let concatenated = (intercalate "\n" contents) ++ "\n"

  -- Pass it through the compiler
  Stdout compiled <- command [Stdin concatenated] compiler []

  -- Write output
  writeFileChanged out compiled
