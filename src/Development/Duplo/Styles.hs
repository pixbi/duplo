module Development.Duplo.Styles (
    build
  ) where

import Control.Monad (filterM)
import Data.List (intercalate)
import Development.Duplo.Utilities (getDirectoryFilesInOrder, logAction)
import Development.Shake
import Development.Shake.FilePath (combine)

build :: FilePath -> FilePath -> FilePath -> Action ()
build cwd bin = \ out -> do
  logAction "Building styles"

  -- These paths don't need to be expanded
  let staticPaths' = [ "app/styl/variables.styl"
                     , "app/styl/keyframes.styl"
                     , "app/styl/fonts.styl"
                     , "app/styl/reset.styl"
                     , "app/styl/main.styl"
                     ]
  staticPaths <- filterM doesFileExist staticPaths'

  -- These paths need to be expanded by Shake
  let dynamicPaths' = [ "app/modules//index.styl"
                      , "components/*/app/styl/variables.styl"
                      , "components/*/app/styl/keyframes.styl"
                      , "components/*/app/styl/fonts.styl"
                      , "components/*/app/styl/reset.styl"
                      , "components/*/app/styl/main.styl"
                      , "components/*/app/modules//index.styl"
                      ]
  dynamicPaths <- getDirectoryFilesInOrder cwd dynamicPaths'

  -- Merge both types of paths
  let paths = staticPaths ++ dynamicPaths
  mapM (putNormal . ("Including " ++)) paths

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
  putNormal ""
