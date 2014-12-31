{-# LANGUAGE ScopedTypeVariables #-}

module Development.Duplo.Scripts where

import Control.Applicative ((<*>), (<$>))
import Control.Exception (throw, SomeException(..))
import Control.Lens hiding (Action)
import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate, nubBy)
import Data.Text.Lazy (Text, pack, unpack, replace, splitOn)
import Development.Duplo.Component (extractCompVersions)
import Development.Duplo.Files (File(..), pseudoFile)
import Development.Duplo.JavaScript.Order (order)
import Development.Duplo.Types.JavaScript
import Development.Duplo.Utilities (logStatus, headerPrintSetter, expandPaths, compile, createIntermediaryDirectories, CompiledContent, expandDeps)
import Development.Shake
import Development.Shake.FilePath ((</>))
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
import Text.Regex (mkRegex, matchRegex)
import qualified Development.Duplo.Types.Config as TC
import qualified Language.JavaScript.Parser as JS
import Data.Text.Format (left)
import Data.Text.Lazy.Builder (toLazyText)

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
  liftIO $ logStatus headerPrintSetter "Building scripts"

  let cwd         = config ^. TC.cwd
  let util        = config ^. TC.utilPath
  let env         = config ^. TC.env
  let input       = config ^. TC.input
  let devPath     = config ^. TC.devPath
  let depsPath    = config ^. TC.depsPath
  let devCodePath = devPath </> "modules/index.js"
  let depIds      = config ^. TC.dependencies
  let inDev       = TC.isInDev config
  let inTest      = TC.isInTest config

  -- Preconditions
  lift $ createIntermediaryDirectories devCodePath

  -- These paths don't need to be expanded.
  let staticPaths = case env of
                      "dev"  -> [ "dev/index" ]
                      "test" -> [ "test/index" ]
                      _      -> []
                    ++ [ "app/index" ]

  -- These paths need to be expanded by Shake.
  let depsToExpand id = [ "components/" ++ id ++ "/app/modules" ]
  -- Compile dev files in dev mode as well, taking precendence.
  let dynamicPaths = case env of
                       "dev"  -> [ "dev/modules" ]
                       "test" -> [ "test/modules" ]
                       _      -> []
                     -- Then normal scripts
                     ++ [ "app/modules" ]
                     -- Build list only for dependencies.
                     ++ expandDeps depIds depsToExpand

  -- Merge both types of paths
  paths <- lift $ expandPaths cwd ".js" staticPaths dynamicPaths

  -- Make sure we hvae at least something
  let duploIn = if length input > 0 then input else ""

  -- Figure out each component's version
  compVers <- liftIO $ extractCompVersions cwd

  -- Inject global/environment variables
  let envVars = "var DUPLO_ENV = '" ++ env ++ "';\n"
             -- Decode and parse in runtime to avoid having to deal with
             -- escaping.
             ++ "var DUPLO_IN = JSON.parse(window.atob('" ++ duploIn ++ "') || '{}' );\n"
             ++ "var DUPLO_VERSIONS = " ++ compVers ++ ";\n"

  -- Configure the compiler
  let compiler = if   inDev || inTest
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

-- | Given the original content as string and an error message that is
-- produced by `language-javascript` parser, throw an error.
handleParseError :: String -> String -> String
handleParseError content e = exception
  where
    linedContent = fmap unpack $ splitOn "\n" $ pack content
    lineCount = length linedContent
    lineNum = readParseError e
    -- Display surrounding lines
              -- Construct a list of target line numbers
    lineRange = take errorDisplayRange
              -- Turn into infinite list
              $ iterate (+ 1)
              -- Position the starting point
              $ lineNum - errorDisplayRange `div` 2
    showBadLine' = showBadLine linedContent lineNum
    -- Keep the line number in the possible domain.
    keepInRange = (max 0) . (min lineCount)
    badLines = fmap (showBadLine' . keepInRange) lineRange
    -- Make sure we de-duplicate the lines.
    dedupe = nubBy $ \x y -> fst x == fst y
    -- Extract just the lines for display.
    badLinesDeduped = map snd $ dedupe badLines
    -- Construct the exception.
    exception = throw $
      ShakeException { shakeExceptionTarget = ""
                     , shakeExceptionStack  = []
                     , shakeExceptionInner  = SomeException
                                            $ ParseException
                                            $ badLinesDeduped
                     }

-- | Given a file's lines, its line number, and the "target" line number
-- that caused the parse error, format it for human-readable output.
showBadLine :: [String] -> LineNumber -> LineNumber -> (LineNumber, String)
showBadLine allLines badLineNum lineNum = (lineNum, line')
  where
    line     = allLines !! lineNum
    -- Natural numbering for humans
    toString = unpack . toLazyText
    lineNum' = toString $ left 4 ' ' $ lineNum + 1
    marker   = if   lineNum == badLineNum
               then ">> " ++ lineNum'
               else "   " ++ lineNum'
    line'    = marker ++ " | " ++ line

-- | Because the parser's error isn't readable, we need to use RegExp to
-- extract what we need for debugging.
readParseError :: String -> LineNumber
readParseError e =
    case match of
      Just m  -> (read $ head m) :: Int
      Nothing -> throw $ InternalParserException e
  where
    regex = mkRegex "TokenPn [0-9]+ ([0-9]+) [0-9]+"
    match = matchRegex regex e
