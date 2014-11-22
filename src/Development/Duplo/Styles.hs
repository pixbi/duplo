module Development.Duplo.Styles
  ( build
  ) where

import Development.Duplo.Utilities
         ( getDirectoryFilesInOrder
         , logAction
         , expandPaths
         , compile
         , createIntermediaryDirectories
         , CompiledContent
         )
import Development.Shake
import Development.Shake.FilePath ((</>))
import qualified Development.Duplo.Types.Config as C
import Control.Lens hiding (Action)
import Control.Monad.Trans.Class (lift)

build :: C.BuildConfig
      -> FilePath
      -> CompiledContent ()
build config = \ out -> do
  lift $ logAction "Building styles"

  let cwd         = config ^. C.cwd
  let utilPath    = config ^. C.utilPath
  let devPath     = config ^. C.devPath
  let devCodePath = devPath </> "modules/index.styl"

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- These paths don't need to be expanded
  let staticPaths = [ "app/styl/variables.styl"
                    , "app/styl/keyframes.styl"
                    , "app/styl/fonts.styl"
                    , "app/styl/reset.styl"
                    , "app/styl/main.styl"
                    ]

  -- These paths need to be expanded by Shake
  let dynamicPaths = [ "components/*/app/styl/variables.styl"
                     , "components/*/app/styl/keyframes.styl"
                     , "components/*/app/styl/fonts.styl"
                     , "components/*/app/styl/reset.styl"
                     , "components/*/app/styl/main.styl"
                     , "app/modules//*.styl"
                     , "components/*/app/modules//*.styl"
                     -- Compile dev files in dev mode as well.
                     ] ++ if   C.isInDev config
                          then ["dev/modules//*.styl"]
                          else []

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = utilPath </> "styles.sh"

  -- Compile it
  compiled <- compile config compiler [] paths (return . id) (return . id)

  -- Write it to disk
  lift $ writeFileChanged out compiled
