module Development.Duplo.Utilities
  ( getDirectoryFilesInOrder
  , logAction
  , expandPaths
  , compile
  , FileProcessor
  ) where

import Prelude hiding (readFile)
import Control.Monad (filterM)
import Data.List (intercalate)
import Development.Shake hiding (readFile)
import Development.Duplo.Files
         ( readFile
         , File(..)
         , fileContent
         )
import Development.Shake.FilePath ((</>))
import Control.Lens hiding (Action)
import qualified Development.Duplo.Config as C
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)

type FileProcessor = [File] -> [File]
type StringProcessor = String -> String

getDirectoryFilesInOrder :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFilesInOrder base patterns =
    -- Re-package the contents into the list of paths that we've wanted
    fmap concat files
  where
    -- We need to turn all elements into lists for each to be run independently
    patterns' = fmap (replicate 1) patterns
    -- Curry the function that gets the files given a list of paths
    getFiles  = getDirectoryFiles base
    -- Map over the list monadically to get the paths in order
    files     = mapM getFiles patterns'

logAction :: String -> Action ()
logAction log = do
  putNormal $ ">> " ++ log
  putNormal ""

-- | Given the path to a compiler, parameters to the compiler, a list of
-- paths of to-be-compiled files, the output file path, and a processing
-- function, do the following:
--
-- * reads all the to-be-compiled files
-- * calls the processor with the list of files to perform any
--   pre-processing
-- * concatenates all files
-- * passes the concatenated string to the compiler
-- * returns the compiled content
compile :: C.BuildConfig
        -- The path to the compilation command
        -> FilePath
        -- The parameters passed to the compilation command
        -> [String]
        -- Files to be compiled
        -> [FilePath]
        -- The processing lambda: it is handed with a list of files.
        -> FileProcessor
        -- The postpcessing lambda: it is handed with all content
        -- concatenated.
        -> StringProcessor
        -- The compiled content
        -> MaybeT Action String
compile config compiler params paths preprocess postprocess = do
  mapM (lift . putNormal . ("Including " ++)) paths

  let cwd    = config ^. C.cwd
  let util   = config ^. C.utilPath
  let nodejs = config ^. C.nodejsPath

  -- Construct files
  files <- mapM (readFile cwd) paths

  -- Pass to processor for specific manipulation
  let processed = preprocess files

  -- We only care about the content from this point on
  let contents = fmap (^. fileContent) processed

  -- Trailing newline is significant in case of empty Stylus
  let concatenated = (intercalate "\n" contents) ++ "\n"

  -- Send string over to post-processor in case of any manipulation before
  -- handing off to the compiler. Add trailing newline for hygiene.
  let postprocessed = (++ "\n") $ postprocess concatenated

  -- Paths should be available as environment variables
  envOpt <- addEnv [ ("DUPLO_UTIL", util)
                   , ("DUPLO_NODEJS", nodejs)
                   , ("DUPLO_CWD", cwd)
                   ]

  lift $ putNormal $ "Compiling with: "
                  ++ compiler
                  ++ " "
                  ++ intercalate " " params
  -- Pass it through the compiler
  Stdout compiled <- lift $ command [Stdin postprocessed, envOpt] compiler params

  -- The output
  return compiled

expandPaths :: String -> [String] -> [String] -> Action [String]
expandPaths cwd staticPaths dynamicPaths = do
  let expand = map (cwd </>)
  staticExpanded <- filterM doesFileExist $ expand staticPaths
  dynamicExpanded <- getDirectoryFilesInOrder cwd dynamicPaths
  return $ staticExpanded ++ expand dynamicExpanded
