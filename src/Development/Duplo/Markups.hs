module Development.Duplo.Markups
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
import Development.Duplo.Files
         ( File(..)
         , filePath
         , fileDir
         , fileName
         , componentId
         , fileContent
         , isRoot
         , ComponentId
         )
import Development.Duplo.ComponentIO (parseComponentId)
import System.FilePath.Posix (makeRelative, splitDirectories, joinPath)
import Control.Lens hiding (Action)
import qualified Development.Duplo.Config as C
import Control.Lens hiding (Action)
import Development.Duplo.FileList (collapseFileList, makeFile)
import qualified Development.Duplo.FileList as FileList (filePath)
import Data.String.Utils (replace)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)

build :: C.BuildConfig
      -> FilePath
      -> MaybeT Action ()
build config = \ out -> do
  lift $ logAction "Building markups"
  -- TODO: using Jade's include system means that all files are loaded by
  -- the Jade compiler and Shake has no visibility into file state. We
  -- cannot cache anything that is Jade. Perhaps we could compile the Jade
  -- ourselves?
  lift $ alwaysRerun

  let cwd          = config ^. C.cwd
  let nodejsPath   = config ^. C.nodejsPath
  let devPath      = config ^. C.devPath
  let assetsPath   = config ^. C.assetsPath
  let defaultsPath = config ^. C.defaultsPath
  let targetPath   = config ^. C.targetPath
  let refTagsPath  = defaultsPath </> "head.html"

  -- These paths don't need to be expanded
  let staticPaths = [ "app/index.jade"
                    ]

  -- These paths need to be expanded by Shake
  let dynamicPaths = [ "components/*/app/index.jade"
                     -- Compile dev files in dev mode as well.
                     ] ++ if   C.isInDev config
                          then ["dev/modules//*.jade"]
                          else []

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = nodejsPath </> "jade"
  let params   = [ "--pretty"
                 , "--path"
                 -- Jade takes a file and takes its directory as its
                 -- current working directory
                 , cwd </> "index.jade"
                 ]

  -- Compile it
  let pre files = fmap (rewriteIncludes cwd files) files
  let post = id
  compiled <- compile config compiler params paths pre post

  -- Pull index page from dev, assets, then default otherwise, in that order
  let defaultIndex    = makeFile defaultsPath "index.html"
  let possibleSources = [devPath, assetsPath]
  let possibleIndexes = fmap (flip makeFile "index.html") possibleSources
  fromIndex <- lift $ fromMaybe defaultIndex <$> collapseFileList possibleIndexes
  indexContent <- lift $ readFile' $ fromIndex ^. FileList.filePath

  -- Inject compiled code into the index
  let indexWithMarkup = replace "<body>" ("<body>" ++ compiled) indexContent

  -- Inject CSS/JS references
  refTags <- lift $ readFile' refTagsPath
  let indexWithRefs   = replace "</head>" (refTags ++ "</head>") indexWithMarkup

  -- Write it to disk
  lift $ writeFileChanged out indexWithRefs

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
                       Just (user, repo) -> depId
                       -- Use default component ID
                       Nothing -> defaultId
          compPath = if   length compName > 0
                     then "components" </> compName
                     else ""

      -- If it's not an include statement, just pass it thru
      _ -> line
