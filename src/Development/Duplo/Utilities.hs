module Development.Duplo.Utilities where

import           Control.Applicative            ((<$>))
import           Control.Lens.Operators
import           Control.Monad                  (filterM, zipWithM)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Data.List                      (intercalate)
import           Development.Duplo.Files        (File (..), fileContent,
                                                 readFile)
import qualified Development.Duplo.Types.Config as TC
import           Development.Shake              (CmdOption (..))
import qualified Development.Shake              as DS
import           Development.Shake.FilePath     ((</>))
import           Prelude                        hiding (readFile)
import           System.Console.ANSI            (Color (..),
                                                 ColorIntensity (..),
                                                 ConsoleLayer (..), SGR (..),
                                                 setSGR)
import           System.FilePath.Posix          (dropTrailingPathSeparator,
                                                 joinPath, splitPath)

type CompiledContent = MaybeT DS.Action
type FileProcessor = [File] -> CompiledContent [File]
type StringProcessor = String -> CompiledContent String

-- | Construct a file pattern by providing a base directory and an
-- extension.
makePattern :: FilePath -> String -> FilePath
makePattern base extension = base ++ "//*" ++ extension

-- | Construct and return the given base directory and extension whose base
-- directory exists.
makeValidPattern :: FilePath -> String -> DS.Action [FilePath]
makeValidPattern base extension = do
    exists <- DS.doesDirectoryExist base
    let ptrn = makePattern base extension
    return [ ptrn | exists ]

-- | Splice a list of base directories and their corresponding extensions
-- for a list of file patterns.
makeFilePatterns :: [FilePath] -> [String] -> DS.Action [DS.FilePattern]
makeFilePatterns bases exts = do
    patternList <- zipWithM makeValidPattern bases exts
    let conc =  foldl1 (++)
    -- Avoid infinite list.
    let patterns =  if   null patternList
                    then return []
                    else conc patternList
    return patterns

-- | Given a working directory and a list of patterns, expand all the
-- paths, in order.
getDirectoryFilesInOrder :: FilePath -> String -> [DS.FilePattern] -> DS.Action [FilePath]
getDirectoryFilesInOrder base extension patterns = do
    -- Make sure we have a clean base.
    let base' = dropTrailingPathSeparator base
    -- We need to terminate the infinite list.
    let listSize = length patterns
    -- Make extension a list of itself.
    let exts = replicate listSize extension
    -- Turn file patterns into absolute patterns.
    let absPatterns = fmap (base' </>) patterns
    -- Make sure we get all valid file patterns for dynamic paths.
    validPatterns <- makeFilePatterns absPatterns exts
    -- Remove the prefix that was needed for file pattern construction.
    let relPatterns = fmap (drop (length base' + 1)) validPatterns
    -- We need to turn all elements into lists for each to be run independently.
    let patternLists = fmap (replicate 1) relPatterns
    -- Curry the function that gets the files given a list of paths.
    let getFiles = getDirectoryFiles base'
    -- Map over the list monadically to get the paths in order.
    allFiles <- mapM getFiles patternLists
    -- Re-package the contents into the list of paths that we've wanted.
    let files = concat $ filter (not . null) allFiles
    -- We're all set
    return files

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
  mapM_ (lift . DS.putNormal . ("Including " ++)) paths

  let cwd = config ^. TC.cwd

  -- Construct files
  files <- mapM (readFile cwd) paths

  -- Pass to processor for specific manipulation
  processed <- preprocess files

  -- We only care about the content from this point on
  let contents = fmap (^. fileContent) processed

  -- Trailing newline is significant in case of empty Stylus
  let concatenated = intercalate "\n" contents ++ "\n"

  -- Send string over to post-processor in case of any manipulation before
  -- handing off to the compiler. Add trailing newline for hygiene.
  postprocessed <- (++ "\n") <$> postprocess concatenated

  -- Paths should be available as environment variables
  envOpt <- createStdEnv config

  lift $ DS.putNormal $  "Compiling with: "
                      ++ compiler
                      ++ " "
                      ++ unwords params
  -- Pass it through the compiler
  DS.Stdout compiled <-
    lift $ DS.command [DS.Stdin postprocessed, envOpt] compiler params

  -- The output
  return compiled

-- | Given a list of static and a list of dynamic paths, return a list of
-- all paths, resolved to absolute paths.
expandPaths :: FilePath -> String -> [FilePath] -> [FilePath] -> DS.Action [FilePath]
expandPaths cwd extension staticPaths dynamicPaths = do
  let expandStatic  =  map (\p -> cwd </> p ++ extension)
  let expandDynamic =  map (cwd </>)
  staticExpanded    <- filterM DS.doesFileExist $ expandStatic staticPaths
  dynamicExpanded   <- getDirectoryFilesInOrder cwd extension dynamicPaths
  return $ staticExpanded ++ expandDynamic dynamicExpanded

-- | Given a list of paths, make sure all intermediary directories are
-- there.
createPathDirectories :: [FilePath] -> DS.Action ()
createPathDirectories paths = do
  let mkdir dir = DS.command_ [] "mkdir" ["-p", dir]
  existing <- filterM (fmap not . DS.doesDirectoryExist) paths
  mapM_ mkdir existing

-- | Create all the directories within a path if they do not exist. Note
-- that the last segment is assumed to be the file and therefore not
-- created.
createIntermediaryDirectories :: String -> DS.Action ()
createIntermediaryDirectories path =
    DS.command_ [] "mkdir" ["-p", dir]
  where
    dir = joinPath $ init $ splitPath path

-- | Return a list of dynamic paths given a list of dependency ID and
-- a function to expand one ID into a list of paths.
expandDeps :: [String] -> (String -> [FilePath]) -> [FilePath]
expandDeps deps expander = concat $ fmap expander deps

-- | Shake hangs when the path given to `getDirectoryFiles` doesn't exist.
-- This is a safe version of that.
getDirectoryFiles :: FilePath -> [DS.FilePattern] -> DS.Action [FilePath]
getDirectoryFiles base patterns = do
    exist <- DS.doesDirectoryExist base
    if   exist
    then DS.getDirectoryFiles base patterns
    else return []

-- | Error printer: white text over red background.
errorPrintSetter :: IO ()
errorPrintSetter = setSGR [ SetColor Background Vivid Red
                          , SetColor Foreground Vivid White
                          ]

-- | Header printer: blue text
headerPrintSetter :: IO ()
headerPrintSetter = setSGR [ SetColor Foreground Vivid Magenta ]

-- | Success printer: white text over green background
successPrintSetter :: IO ()
successPrintSetter = setSGR [ SetColor Background Vivid Green
                            , SetColor Foreground Vivid White
                            ]

-- | Log a message with a provided print configuration setter.
logStatus :: IO () -> String -> IO ()
logStatus printSetter message = do
  printSetter
  putStr $ "\n>> " ++ message
  setSGR [Reset]
  putStrLn ""

-- | Put together common (i.e. standard) environment variables.
createStdEnv :: MonadIO m => TC.BuildConfig -> m CmdOption
createStdEnv config = do
  let cwd    = config ^. TC.cwd
  let util   = config ^. TC.utilPath
  let nodejs = config ^. TC.nodejsPath
  let misc   = config ^. TC.miscPath
  let target = config ^. TC.targetPath
  let duplo  = config ^. TC.duploPath
  let test   = config ^. TC.testPath
  let app    = config ^. TC.appPath
  let deps   = config ^. TC.depsPath
  let dev    = config ^. TC.devPath

  DS.addEnv [ ("DUPLO_UTIL", util)
            , ("DUPLO_NODEJS", nodejs)
            , ("DUPLO_CWD", cwd)
            , ("DUPLO_MISC", misc)
            , ("DUPLO_TARGET", target)
            , ("DUPLO_PATH", duplo)
            , ("DUPLO_TEST", test)
            , ("DUPLO_APP", app)
            , ("DUPLO_DEPS", deps)
            , ("DUPLO_DEV", dev)
            ]
