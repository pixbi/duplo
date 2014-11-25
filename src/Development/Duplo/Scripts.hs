{-# LANGUAGE ScopedTypeVariables #-}

module Development.Duplo.Scripts
  ( build
  ) where

import Control.Applicative ((<*>), (<$>))
import Control.Exception (throw, SomeException(..))
import Control.Lens hiding (Action)
import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Data.Text.Lazy (Text, pack, unpack, replace, splitOn)
import Development.Duplo.Component (extractCompVersions)
import Development.Duplo.Files (File(..), pseudoFile)
import Development.Duplo.JavaScript.Order (order)
import Development.Duplo.Types.JavaScript
import Development.Duplo.Utilities (getDirectoryFilesInOrder, logAction, expandPaths, compile, createIntermediaryDirectories, CompiledContent, expandDeps)
import Development.Shake
import Development.Shake.FilePath ((</>))
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
import Text.Regex (mkRegex, matchRegex)
import qualified Development.Duplo.Types.Config as TC
import qualified Language.JavaScript.Parser as JS

-- | How many lines to display around the source of error (both ways).
errorDisplayRange :: Int
errorDisplayRange = 20

-- | Build scripts
      -- The environment
build :: TC.BuildConfig
      -- The output file
      -> FilePath
      -- Doesn't need anything in return
      -> CompiledContent ()
build config = \ out -> do
  lift $ logAction "Building scripts"

  let cwd         = config ^. TC.cwd
  let util        = config ^. TC.utilPath
  let env         = config ^. TC.env
  let input       = config ^. TC.input
  let devPath     = config ^. TC.devPath
  let devCodePath = devPath </> "modules/index.js"
  let depIds      = config ^. TC.dependencies

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- These paths don't need to be expanded.
  let staticPaths = [ "app/index.js" ]

  -- These paths need to be expanded by Shake.
  let expandDepsA id = ["components/" ++ id ++ "/app/modules//*.js"]
  let dynamicPaths = [ "app/modules//*.js" ]
                  -- Build list only for dependencies.
                  ++ expandDeps depIds expandDepsA
                  -- Compile dev files in dev mode as well.
                  ++ if TC.isInDev config then ["dev/modules//*.js"] else []

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd staticPaths dynamicPaths

  -- Sanitize input
  let duploIn = sanitize input
  -- Make sure we hvae at least something
  let duploIn' = if length duploIn > 0 then duploIn else "{}"

  -- Figure out each component's version
  compVers <- liftIO $ extractCompVersions cwd

  -- Inject global/environment variables
  let envVars = "var DUPLO_ENV = '" ++ env ++ "';\n"
             ++ "var DUPLO_IN = " ++ duploIn' ++ ";\n"
             ++ "var DUPLO_VERSIONS = " ++ compVers ++ ";\n"

  -- Configure the compiler
  let compiler = if   TC.isInDev config
                 then util </> "scripts-dev.sh"
                 else util </> "scripts-optimize.sh"

  -- Create a pseudo file that contains the environment variables and
  -- prepend the file.
  let pre = return . (:) (pseudoFile { _fileContent = envVars })
  -- Reorder modules and print as string
  let prepareJs = JS.renderToString . order
  let post content = return
                   -- Handle error
                   $ either (handleParseError content) prepareJs
                   -- Parse
                   $ JS.parse content ""

  -- Build it
  compiled <- compile config compiler [] paths pre post

  -- Write it to disk
  lift $ writeFileChanged out compiled

-- | Sanitize script input
sanitize :: String -> String
sanitize = unpack . sanitize' . pack

-- | Text version of `sanitize`
sanitize' :: Text -> Text
sanitize' input = foldr id input
               -- Curry sanitizing functions
               $ replace <$> ["\n", "'", "\\"]
                         <*> ["", "\\", "\\\\"]

-- | Given the original content as string and an error message that is
-- produced by `language-javascript` parser, throw an error.
handleParseError :: String -> String -> String
handleParseError content e =
    throw $ ShakeException { shakeExceptionTarget = ""
                           , shakeExceptionStack = []
                           , shakeExceptionInner = SomeException
                                                 $ ParseException
                                                 $ badLines
                           }
  where
    linedContent = fmap unpack $ splitOn "\n" $ pack content
    lineNum = readParseError e
    -- Display surrounding lines
              -- Construct a list of target line numbers
    lineRange = take errorDisplayRange
              -- Turn into infinite list
              $ iterate (+ 1)
              -- Position the starting point
              $ lineNum - errorDisplayRange `div` 2
    badLines = fmap (showBadLine linedContent lineNum) lineRange

-- | Given a file's lines, its line number, and the "target" line number
-- that caused the parse error, format it for human-readable output.
showBadLine :: [String] -> LineNumber -> LineNumber -> String
showBadLine allLines badLineNum lineNum =
    marker ++ " | " ++ line
  where
    line = allLines !! lineNum
    lineNum' = show lineNum
    marker = if   lineNum == badLineNum
             then ">> " ++ lineNum'
             else "   " ++ lineNum'

-- | Because the parser's error isn't readable, we need to use RegExp to
-- extract what we need for debugging.
readParseError :: String -> LineNumber
readParseError e =
    case match of
      Just m -> (read $ head m) :: Int
      Nothing -> throw $ InternalParserException e
  where
    regex = mkRegex "TokenPn [0-9]+ ([0-9]+) [0-9]+"
    match = matchRegex regex e
