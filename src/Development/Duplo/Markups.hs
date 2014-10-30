module Development.Duplo.Markups
  ( build
  ) where

import Development.Duplo.Utilities
         ( getDirectoryFilesInOrder
         , logAction
         , expandPaths
         , buildWith
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

build :: C.BuildConfig
      -> FilePath
      -> Action ()
build config = \ out -> do
  logAction "Building markups"

  let cwd   = config ^. C.cwd
  let bin   = config ^. C.bin

  -- These paths don't need to be expanded
  let staticPaths = [ "app/index.jade"
                    ]

  -- These paths need to be expanded by Shake
  let dynamicPaths = [ "app/*//*.jade"
                     , "components/*/app/*//*.jade"
                     ]

  -- Merge both types of paths
  paths <- expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = bin </> "jade"
  let params   = [ "--pretty"
                 , "--path"
                 -- Jade takes a file and takes its directory as its
                 -- current working directory
                 , cwd </> "index.jade"
                 ]

  -- Build it
  buildWith config compiler params paths out $ \ files ->
    fmap (rewriteIncludes cwd files) files

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
