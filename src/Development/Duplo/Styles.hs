module Development.Duplo.Styles
  ( build
  ) where

import Development.Duplo.Utilities
         ( getDirectoryFilesInOrder
         , logAction
         , expandPaths
         , compile
         )
import Development.Shake
import Development.Shake.FilePath ((</>))
import qualified Development.Duplo.Config as C
import Control.Lens hiding (Action)

build :: C.BuildConfig
      -> FilePath
      -> Action ()
build config = \ out -> do
  logAction "Building styles"

  let cwd   = config ^. C.cwd
  let bin   = config ^. C.bin

  -- These paths don't need to be expanded
  let staticPaths = [ "app/styl/variables.styl"
                    , "app/styl/keyframes.styl"
                    , "app/styl/fonts.styl"
                    , "app/styl/reset.styl"
                    , "app/styl/main.styl"
                    ]

  -- These paths need to be expanded by Shake
  let dynamicPaths = [ "app/modules//index.styl"
                     , "components/*/app/styl/variables.styl"
                     , "components/*/app/styl/keyframes.styl"
                     , "components/*/app/styl/fonts.styl"
                     , "components/*/app/styl/reset.styl"
                     , "components/*/app/styl/main.styl"
                     , "components/*/app/modules//index.styl"
                     , "components/*/app/modules//*.styl"
                     ]

  -- Merge both types of paths
  paths <- expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = bin </> "stylus"

  -- Compile it
  compiled <- compile config compiler [] paths id

  -- Write it to disk
  writeFileChanged out compiled
