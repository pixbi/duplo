module Development.Duplo.Styles
  ( build
  ) where

import Development.Duplo.Utilities
         ( getDirectoryFilesInOrder
         , logAction
         , expandPaths
         , buildWith
         )
import Development.Shake
import Development.Shake.FilePath (combine)

build :: FilePath -> FilePath -> FilePath -> Action ()
build cwd bin = \ out -> do
  logAction "Building styles"

  -- These paths don't need to be expanded
  let staticPaths = [ "app/styl/variables.styl"
                    , "app/styl/keyframes.styl"
                    , "app/styl/fonts.styl"
                    , "app/styl/reset.styl"
                    , "app/styl/main.styl"
                    ]

  -- These paths need to be expanded by Shake
  -- TODO: exclude dependencies not listed in the current mode
  let dynamicPaths = [ "app/modules//index.styl"
                     , "components/*/app/styl/variables.styl"
                     , "components/*/app/styl/keyframes.styl"
                     , "components/*/app/styl/fonts.styl"
                     , "components/*/app/styl/reset.styl"
                     , "components/*/app/styl/main.styl"
                     , "components/*/app/modules//index.styl"
                     ]

  -- Merge both types of paths
  paths <- expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = combine bin "stylus"

  -- Build it
  buildWith compiler [] paths out id
