module Development.Duplo.Utilities
  ( getDirectoryFilesInOrder
  , logAction
  , expandPaths
  , compile
  , FileProcessor
  , createIntermediaryDirectories
  , createPathDirectories
  , CompiledContent
  , expandDeps
  ) where

import Control.Lens hiding (Action)
import Control.Monad (filterM)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Development.Duplo.Files (readFile, File(..), fileContent)
import Development.Shake hiding (readFile)
import Development.Shake.FilePath ((</>))
import Prelude hiding (readFile)
import System.FilePath.Posix (joinPath, splitPath)
import qualified Development.Duplo.Component as CM
import qualified Development.Duplo.Types.AppInfo as AI
import qualified Development.Duplo.Types.Config as TC
import Control.Monad (zipWithM)
import Data.List (isSuffixOf)

type CompiledContent = ExceptT String Action
type FileProcessor = [File] -> CompiledContent [File]
type StringProcessor = String -> CompiledContent String

-- | Construct a file pattern by providing a base directory and an
-- extension.
makePattern :: FilePath -> String -> FilePath
makePattern base extension = base ++ "//*" ++ extension

-- | Construct and return the given base directory and extension whose base
-- directory exists.
makeValidPattern :: FilePath -> String -> Action [FilePath]
makeValidPattern base extension = do
    exists <- doesDirectoryExist base
    let ptrn = makePattern base extension
    return $ if exists then [ptrn] else []

-- | Splice a list of base directories and their corresponding extensions
-- for a list of file patterns.
makeFilePatterns :: [FilePath] -> [String] -> Action [FilePattern]
makeFilePatterns bases exts = do
    patternList  <- zipWithM makeValidPattern bases exts
    let concat   =  foldl1 (++)
    -- Avoid infinite list.
    let patterns =  if   null patternList
                    then return []
                    else concat patternList
    return patterns

-- | Given a working directory and a list of patterns, expand all the
-- paths, in order.
getDirectoryFilesInOrder :: FilePath -> String -> [FilePattern] -> Action [FilePath]
getDirectoryFilesInOrder base extension patterns = do
    -- We need to terminate the infinite list.
    let listSize = length patterns
    -- Make extension a list of itself.
    let exts = take listSize $ repeat extension
    -- Turn file patterns into absolute patterns.
    let absPatterns = fmap (base </>) patterns
    -- Make sure we get all valid file patterns for dynamic paths.
    validPatterns <- makeFilePatterns absPatterns exts
    -- Remove the prefix that was needed for file pattern construction.
    let relPatterns = fmap (drop (length base + 1)) validPatterns
    -- We need to turn all elements into lists for each to be run independently.
    let patternLists = fmap (replicate 1) relPatterns
    -- Curry the function that gets the files given a list of paths.
    let getFiles = getDirectoryFiles base
    -- Map over the list monadically to get the paths in order.
    allFiles <- mapM getFiles patternLists
    -- Re-package the contents into the list of paths that we've wanted.
    let files = concat $ filter (not . null) allFiles
    -- We're all set
    return files

logAction :: String -> Action ()
logAction log = do
  putNormal $ "\n>> " ++ log

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
compile :: TC.BuildConfig
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
        -> CompiledContent String
compile config compiler params paths preprocess postprocess = do
  mapM (lift . putNormal . ("Including " ++)) paths

  let cwd    = config ^. TC.cwd
  let util   = config ^. TC.utilPath
  let nodejs = config ^. TC.nodejsPath

  -- Construct files
  files <- mapM (readFile cwd) paths

  -- Pass to processor for specific manipulation
  processed <- preprocess files

  -- We only care about the content from this point on
  let contents = fmap (^. fileContent) processed

  -- Trailing newline is significant in case of empty Stylus
  let concatenated = (intercalate "\n" contents) ++ "\n"

  -- Send string over to post-processor in case of any manipulation before
  -- handing off to the compiler. Add trailing newline for hygiene.
  postprocessed <- fmap (++ "\n") $ postprocess concatenated

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

-- | Given a list of static and a list of dynamic paths, return a list of
-- all paths, resolved to absolute paths.
expandPaths :: FilePath -> String -> [FilePath] -> [FilePath] -> Action [FilePath]
expandPaths cwd extension staticPaths dynamicPaths = do
  let expandStatic  =  map (\p -> cwd </> p ++ extension)
  let expandDynamic =  map (cwd </>)
  staticExpanded    <- filterM doesFileExist $ expandStatic staticPaths
  dynamicExpanded   <- getDirectoryFilesInOrder cwd extension dynamicPaths
  return $ staticExpanded ++ expandDynamic dynamicExpanded

-- | Given a list of paths, make sure all intermediary directories are
-- there.
createPathDirectories :: [FilePath] -> Action ()
createPathDirectories paths = do
  let mkdir = \ dir -> command_ [] "mkdir" ["-p", dir]
  existing <- filterM ((fmap not) . doesDirectoryExist) paths
  mapM_ mkdir existing

-- | Create all the directories within a path if they do not exist. Note
-- that the last segment is assumed to be the file and therefore not
-- created.
createIntermediaryDirectories :: String -> Action ()
createIntermediaryDirectories path =
    command_ [] "mkdir" ["-p", dir]
  where
    dir = joinPath $ init $ splitPath path

-- | Return a list of dynamic paths given a list of dependency ID and
-- a function to expand one ID into a list of paths.
expandDeps :: [String] -> (String -> [FilePath]) -> [FilePath]
expandDeps deps expander = concat $ (fmap expander deps)
