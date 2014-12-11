module Development.Duplo.Markups where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Lens hiding (Action)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.String.Utils (replace)
import Development.Duplo.Component (parseComponentId)
import Development.Duplo.FileList (collapseFileList, makeFile)
import Development.Duplo.Files (File(..), filePath, fileDir, fileName, componentId, fileContent, isRoot, ComponentId)
import Development.Duplo.Utilities (logStatus, headerPrintSetter, expandPaths, compile, createIntermediaryDirectories, CompiledContent, expandDeps)
import Development.Shake
import Development.Shake.FilePath ((</>))
import System.FilePath.Posix (makeRelative, splitDirectories, joinPath)
import qualified Development.Duplo.FileList as FileList (filePath)
import qualified Development.Duplo.Types.Builder as BD
import qualified Development.Duplo.Types.Config as TC

build :: TC.BuildConfig
      -> FilePath
      -> CompiledContent ()
build config = \ out -> do
  liftIO $ logStatus headerPrintSetter "Building markups"
  -- TODO: using Jade's include system means that all files are loaded by
  -- the Jade compiler and Shake has no visibility into file state. We
  -- cannot cache anything that is Jade. Perhaps we could compile the Jade
  -- ourselves?
  lift $ alwaysRerun

  let cwd           = config ^. TC.cwd
  let env           = config ^. TC.env
  let utilPath      = config ^. TC.utilPath
  let devPath       = config ^. TC.devPath
  let appPath       = config ^. TC.appPath
  let testPath      = config ^. TC.testPath
  let assetsPath    = config ^. TC.assetsPath
  let defaultsPath  = config ^. TC.defaultsPath
  let refTagsPath   = defaultsPath </> "head.html"
  let devAssetsPath = devPath </> "assets"
  let devCodePath   = devPath </> "modules/index"
  let depIds        = config ^. TC.dependencies

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- Expand all paths
  let depExpander id = [ "components" </> id </> "app/modules/index" ]
  let expanded       = expandDeps depIds depExpander
  let allPaths       = [ "app/modules/index" ] ++ expanded
  let absPaths       = case env of
                         "dev"  -> [ devCodePath ]
                         "test" -> [ testPath ]
                         _      -> []
                       ++ map (cwd </>) allPaths

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd ".jade" absPaths []

  -- Compiler details
  let compiler = utilPath </> "markups-compile.sh"
  let preCompile files = return $ fmap (rewriteIncludes cwd files) files

  -- Compile content
  compiled <- compile config compiler [] paths preCompile (return . id)

  -- Pull index page from either dev, assets, or default otherwise, in that
  -- order.
  let possibleSources =  fmap (</> "index.jade") [ devPath
                                                 , appPath
                                                 , defaultsPath
                                                 ]

  -- Compile the index file
  compiledIndex <- compile config compiler [] ["app/index.jade"] preCompile (return . id)

  -- Inject compiled code into the index
  let indexWithMarkup = replace "<body>" ("<body>" ++ compiled) compiledIndex

  -- Inject CSS/JS references
  refTags <- lift $ readFile' refTagsPath
  let indexWithRefs = replace "</head>" (refTags ++ "</head>") indexWithMarkup

  -- Path to the minifier
  let minifier = utilPath </> "markups-minify.sh"
  -- Minify it
  let postMinify _ = return indexWithRefs
  minified <- compile config minifier [] paths (return . id) postMinify

  -- Write it to disk
  lift $ writeFileChanged out minified

-- | Rewrite paths to external files (i.e. include statements) because Jade
-- doesn't accept more than one path to look up includes. It is passed all
-- the files to be compiled and a file whose include statements are to be
-- rewritten.
                -- The current working directory
rewriteIncludes :: FilePath
                -- All the files to be compiled
                -> [File]
                -- The current file
                -> File
                -- The same file with include statements rewritten
                -> File
rewriteIncludes cwd files file =
    file & fileContent .~ rewritten
  where
    path       = file ^. filePath
    dir        = file ^. fileDir
    name       = file ^. fileName
    id         = file ^. componentId
    content    = file ^. fileContent
    isRoot'    = file ^. isRoot
    defaultId  = if isRoot' then "" else id
    content'   = Prelude.lines content
    rewritten' = fmap (rewriteInclude defaultId) content'
    rewritten  = Prelude.unlines rewritten'

-- | Given a component ID and a content line, rewrite if it is an include
-- statement to be relative to project root
rewriteInclude :: ComponentId -> String -> String
rewriteInclude defaultId line =
  let
    -- Find how many spaces precede the line
    padLength  = length $ takeWhile (' ' ==) line
    padding    = replicate padLength ' '
    -- The statement in tokens
    tokens     = words line
    -- Further tokenize the tokens as paths
    tokenPaths = fmap splitDirectories tokens
  in
    case tokenPaths of
      -- Deconstruct an include statement
      -- TODO: the following needs some refactoring and/or comments. This
      -- section is very confusing.
      (("include":_) : (depId:relPath) : _) ->
          padding ++ "include " ++ compPath </> relPath'
        where
          compName = case (parseComponentId depId) of
                       -- There is a component ID
                       Right (user, repo) -> depId
                       -- Use default component ID
                       Left _ -> defaultId
          relPath' = if   length compName > 0
                     then ("app/modules" </>) $ joinPath relPath
                     else (("app/modules" </> depId) </>) $ joinPath relPath
          compPath = if   length compName > 0
                     then "components" </> compName
                     else ""

      -- If it's not an include statement, just pass it thru
      _ -> line
