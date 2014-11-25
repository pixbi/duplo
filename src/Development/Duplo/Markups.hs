module Development.Duplo.Markups
  ( build
  ) where

import Control.Applicative ((<$>))
import Control.Lens hiding (Action)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.String.Utils (replace)
import Development.Duplo.Component (parseComponentId)
import Development.Duplo.FileList (collapseFileList, makeFile)
import Development.Duplo.Files (File(..), filePath, fileDir, fileName, componentId, fileContent, isRoot, ComponentId)
import Development.Duplo.Utilities (logAction, expandPaths, compile, createIntermediaryDirectories, CompiledContent, expandDeps)
import Development.Shake
import Development.Shake.FilePath ((</>))
import System.FilePath.Posix (makeRelative, splitDirectories, joinPath)
import qualified Development.Duplo.FileList as FileList (filePath)
import qualified Development.Duplo.Types.Config as TC

build :: TC.BuildConfig
      -> FilePath
      -> CompiledContent ()
build config = \ out -> do
  lift $ logAction "Building markups"
  -- TODO: using Jade's include system means that all files are loaded by
  -- the Jade compiler and Shake has no visibility into file state. We
  -- cannot cache anything that is Jade. Perhaps we could compile the Jade
  -- ourselves?
  lift $ alwaysRerun

  let cwd           = config ^. TC.cwd
  let utilPath      = config ^. TC.utilPath
  let devPath       = config ^. TC.devPath
  let assetsPath    = config ^. TC.assetsPath
  let defaultsPath  = config ^. TC.defaultsPath
  let refTagsPath   = defaultsPath </> "head.html"
  let devAssetsPath = devPath </> "assets"
  let devCodePath   = devPath </> "modules/index.jade"
  let depIds      = config ^. TC.dependencies
  let expandDeps' = expandDeps depIds

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- These paths don't need to be expanded
  let expandDepsStatic id = [ "components/*/app/index.jade" ]
  let staticPaths = [ "app/index.jade" ] ++ (expandDeps' expandDepsStatic)

  -- These paths need to be expanded by Shake
  let dynamicPaths = if   TC.isInDev config
                     then [devCodePath ++ "//*.jade"]
                     else []

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = utilPath </> "markups-compile.sh"

  -- Compile it
  let preCompile files = return $ fmap (rewriteIncludes cwd files) files
  compiled <- compile config compiler [] paths preCompile (return . id)

  -- Pull index page from dev, assets, then default otherwise, in that order.
  let defaultIndex    = makeFile defaultsPath "index.html"
  let possibleSources = [devAssetsPath, assetsPath]
  let possibleIndexes = fmap (flip makeFile "index.html") possibleSources
  fromIndex <- lift $ fromMaybe defaultIndex <$> collapseFileList possibleIndexes
  indexContent <- lift $ readFile' $ fromIndex ^. FileList.filePath

  -- Inject compiled code into the index
  let indexWithMarkup = replace "<body>" ("<body>" ++ compiled) indexContent

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
    padLength  = length $ takeWhile (== ' ') line
    padding    = replicate padLength ' '
    -- The statement in tokens
    tokens     = words line
    -- Further tokenize the tokens as paths
    tokenPaths = fmap splitDirectories tokens
  in
    case tokenPaths of
      -- Deconstruct an include statement
      (("include":_) : (depId:relPath) : _) ->
          padding ++ "include " ++ compPath </> relPath'
        where
          relPath' = (</>) "app/modules" $ joinPath relPath
          compName = case (parseComponentId depId) of
                       -- There is a component ID
                       Right (user, repo) -> depId
                       -- Use default component ID
                       Left _ -> defaultId
          compPath = if   length compName > 0
                     then "components" </> compName
                     else ""

      -- If it's not an include statement, just pass it thru
      _ -> line
