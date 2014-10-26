module Development.Duplo.Markups
  ( build
  ) where

import Development.Duplo.Utilities
         ( getDirectoryFilesInOrder
         , logAction
         , expandPaths
         , buildWith
         )
import Development.Shake hiding (readFile)
import Development.Shake.FilePath (combine)
import Development.Duplo.Files (File, readFile)

build :: FilePath -> FilePath -> FilePath -> Action ()
build cwd bin = \ out -> do
  logAction "Building markups"

  -- These paths don't need to be expanded
  let staticPaths = [ "app/index.jade"
                    ]

  -- These paths need to be expanded by Shake
  -- TODO: exclude dependencies not listed in the current mode
  let dynamicPaths = [ "app/*//*.jade"
                     , "components/*/app/*//*.jade"
                     ]

  -- Merge both types of paths
  paths <- expandPaths cwd staticPaths dynamicPaths

  -- Path to the compiler
  let compiler = combine bin "jade"
  let params   = [ "--pretty"
                 , "--path"
                 , cwd
                 ]

  -- Build it
  buildWith compiler params paths out $ \ files ->
    fmap (rewriteIncludes files) files

-- | Rewrite paths to external files (i.e. include statements) because Jade
-- doesn't accept more than one path to look up includes. It is passed all
-- the files to be compiled and a file whose include statements are to be
-- rewritten.
                -- All the files to be compiled
rewriteIncludes :: [File]
                -- The current file
                -> File
                -- The same file with include statements rewritten
                -> File
rewriteIncludes files file = file
