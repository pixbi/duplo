module Development.Duplo.Markups where

import           Control.Applicative             ((<$>))
import           Control.Exception               (throw)
import           Control.Lens                    hiding (Action)
import           Control.Monad.Trans.Class       (lift)
import           Data.List.Utils                 (replace)
import           Data.Maybe                      (fromMaybe)
import           Development.Duplo.Component     (parseComponentId)
import           Development.Duplo.FileList      (collapseFileList, makeFile)
import qualified Development.Duplo.FileList      as FileList (filePath)
import           Development.Duplo.Files         (ComponentId, File (..),
                                                  componentId, fileContent,
                                                  fileDir, fileName, filePath,
                                                  isRoot)
import qualified Development.Duplo.Types.Builder as BD
import qualified Development.Duplo.Types.Config  as TC
import           Development.Duplo.Utilities     (CompiledContent, compile,
                                                  createIntermediaryDirectories,
                                                  expandDeps, expandPaths,
                                                  headerPrintSetter, logStatus)
import           Development.Shake
import           Development.Shake.FilePath      ((</>))
import           System.Directory                (findFile)
import           System.FilePath.Posix           (joinPath, makeRelative,
                                                  splitDirectories)

build :: TC.BuildConfig
      -> FilePath
      -> CompiledContent ()
build config out = do
  liftIO $ logStatus headerPrintSetter "Building markups"
  -- TODO: using Jade's include system means that all files are loaded by
  -- the Jade compiler and Shake has no visibility into file state. We
  -- cannot cache anything that is Jade. Perhaps we could compile the Jade
  -- ourselves?
  lift alwaysRerun

  let cwd           = config ^. TC.cwd
  let env           = config ^. TC.env
  let buildMode     = config ^. TC.buildMode
  let utilPath      = config ^. TC.utilPath
  let devPath       = config ^. TC.devPath
  let appPath       = config ^. TC.appPath
  let testPath      = config ^. TC.testPath
  let duploPath     = config ^. TC.duploPath
  let assetsPath    = config ^. TC.assetsPath
  let targetPath    = config ^. TC.targetPath
  let defaultsPath  = config ^. TC.defaultsPath
  let refTagsPath   = defaultsPath </> "head.html"
  let devAssetsPath = devPath </> "assets"
  let devCodePath   = devPath </> "modules/index"
  let depIds        = config ^. TC.dependencies
  let inTest        = TC.isInTest config

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- Expand all paths
  let depExpander id = [ "components" </> id </> "app/modules/index" ]
  let expanded       = expandDeps depIds depExpander
  let allPaths       = "app/modules/index" : expanded
  let absPaths       = case buildMode of
                         "developmoent" -> [ devCodePath ]
                         "test"         -> [ targetPath </> "vendor/mocha" ]
                         _              -> []
                       ++ map (cwd </>) allPaths

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd ".jade" absPaths []

  -- Compiler details
  let compiler = utilPath </> "markups-compile.sh"
  let preCompile files = return $ fmap (rewriteIncludes cwd files) files

  -- Compile content
  compiled <- compile config compiler [] paths preCompile return

  -- Pull index page from either dev, assets, or default otherwise, in that
  -- order.
  let possibleSources = [ devPath, appPath, defaultsPath ]
  -- We know at least one would satisfy because there's always one in
  -- the default path
  Just indexFile <- liftIO $ findFile possibleSources "index.jade"

  -- Compile the index file
  compiledIndex <- compile config compiler [] [indexFile] preCompile return

  -- Inject compiled code into the index
  let indexWithMarkup = replace "<body>" ("<body>" ++ compiled) compiledIndex

  -- Inject CSS/JS references
  refTags <- lift $ readFile' refTagsPath
  let indexWithRefs = replace "</head>" (refTags ++ "</head>") indexWithMarkup

  -- Inject CSS/JS references if in testing
  refTagsInTest <- lift $ readFile' (duploPath </> "etc/test/head.html")
  scriptsPaths  <- lift $ expandPaths cwd ".js" [] [ testPath </> "modules"
                                                   , appPath  </> "modules"
                                                   ]

  let buildScriptTag path = "<script defer=\"defer\" src=\"" ++ makeRelative cwd path ++ "\"></script>"
  let scriptsTags         = concatMap buildScriptTag scriptsPaths
  let indexWithTestRefs   = if   inTest
                            then replace "</head>" (refTagsInTest ++ scriptsTags ++ "</head>") indexWithRefs
                            else indexWithRefs

  -- Path to the minifier
  let minifier = utilPath </> "markups-minify.sh"
  -- Minify it
  let postMinify _ = return indexWithTestRefs
  minified <- compile config minifier [] paths return postMinify

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
      (("include":_) : fullPath@(prefix:relPath) : _) ->
          padding ++ "include " ++ resolvedPath
        where
          -- We need to figure out which component's name to use to prefix
          -- the include path. Also get the path prefix in case we're using
          -- the default ID.
          (compName, relPathBase) = case parseComponentId prefix of
                                      -- There is a component ID.
                                      Right (user, repo) -> (prefix, "")
                                      -- Use default component ID
                                      -- otherwise.
                                      Left _ -> (defaultId, prefix)
          resolvedPath = if   not (null compName)
                         -- It's referring to a file inside a component.
                         then "components" </> compName </> "app/modules" </>
                              relPathBase </> joinPath relPath
                         -- It's not referring to a file at the top-level.
                         else "app/modules" </> joinPath fullPath

      -- If it's not an include statement, just pass it thru
      _ -> line
