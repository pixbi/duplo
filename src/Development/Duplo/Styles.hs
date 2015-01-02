module Development.Duplo.Styles where

import Control.Lens hiding (Action)
import Control.Monad.Trans.Class (lift)
import Development.Duplo.Utilities (logStatus, headerPrintSetter, expandPaths, compile, createIntermediaryDirectories, CompiledContent, expandDeps)
import Development.Shake
import Development.Shake.FilePath ((</>))
import qualified Development.Duplo.Types.Config as TC

build :: TC.BuildConfig
      -> FilePath
      -> CompiledContent ()
build config out = do
  liftIO $ logStatus headerPrintSetter "Building styles"

  let cwd         = config ^. TC.cwd
  let utilPath    = config ^. TC.utilPath
  let devPath     = config ^. TC.devPath
  let devCodePath = devPath </> "modules/index.styl"
  let depIds      = config ^. TC.dependencies
  let expandDeps' = expandDeps depIds

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- These paths don't need to be expanded
  let expandDepsStatic id = [ "components/" ++ id ++ "/app/styl/variables"
                            , "components/" ++ id ++ "/app/styl/keyframes"
                            , "components/" ++ id ++ "/app/styl/fonts"
                            , "components/" ++ id ++ "/app/styl/reset"
                            , "components/" ++ id ++ "/app/styl/main"
                            ]
  let staticPaths = [ "app/styl/variables"
                    , "app/styl/keyframes"
                    , "app/styl/fonts"
                    , "app/styl/reset"
                    , "app/styl/main"
                    ]
                 ++ expandDeps' expandDepsStatic

  -- These paths need to be expanded by Shake
  let expandDepsDynamic id = [ "components/" ++ id ++ "/app/modules" ]
  let dynamicPaths = [ "app/modules" ]
                  ++ expandDeps' expandDepsDynamic
                  -- Compile dev files in dev mode as well.
                  ++ [ "dev/modules" | TC.isInDev config ]

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd ".styl" staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = utilPath </> "styles.sh"

  -- Compile it
  compiled <- compile config compiler [] paths return return

  -- Write it to disk
  lift $ writeFileChanged out compiled
