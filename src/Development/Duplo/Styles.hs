module Development.Duplo.Styles
  ( build
  ) where

import Development.Duplo.Utilities (getDirectoryFilesInOrder, logAction, expandPaths, compile, createIntermediaryDirectories, CompiledContent, expandDeps)
import Development.Shake
import Development.Shake.FilePath ((</>))
import qualified Development.Duplo.Types.Config as TC
import Control.Lens hiding (Action)
import Control.Monad.Trans.Class (lift)

build :: TC.BuildConfig
      -> FilePath
      -> CompiledContent ()
build config = \ out -> do
  lift $ logAction "Building styles"

  let cwd         = config ^. TC.cwd
  let utilPath    = config ^. TC.utilPath
  let devPath     = config ^. TC.devPath
  let devCodePath = devPath </> "modules/index.styl"
  let depIds      = config ^. TC.dependencies
  let expandDeps' = expandDeps depIds

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- These paths don't need to be expanded
  let expandDepsStatic id = [ "components/" ++ id ++ "/app/styl/variables.styl"
                            , "components/" ++ id ++ "/app/styl/keyframes.styl"
                            , "components/" ++ id ++ "/app/styl/fonts.styl"
                            , "components/" ++ id ++ "/app/styl/reset.styl"
                            , "components/" ++ id ++ "/app/styl/main.styl"
                            ]
  let staticPaths = [ "app/styl/variables.styl"
                    , "app/styl/keyframes.styl"
                    , "app/styl/fonts.styl"
                    , "app/styl/reset.styl"
                    , "app/styl/main.styl"
                    ]
                 ++ (expandDeps' expandDepsStatic)

  -- These paths need to be expanded by Shake
  let expandDepsDynamic id = [ "components/" ++ id ++ "/app/modules//*.styl" ]
  let dynamicPaths = [ "app/modules//*.styl" ]
                  ++ (expandDeps' expandDepsDynamic)
                  -- Compile dev files in dev mode as well.
                  ++ if TC.isInDev config then ["dev/modules//*.styl"] else []

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = utilPath </> "styles.sh"

  -- Compile it
  compiled <- compile config compiler [] paths (return . id) (return . id)

  -- Write it to disk
  lift $ writeFileChanged out compiled
