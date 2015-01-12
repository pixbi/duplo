{-# LANGUAGE DeriveDataTypeable #-}

module Development.Duplo.Types.JavaScript where

import           Control.Exception          (Exception)
import           Control.Monad.State.Lazy   (State)
import           Data.List                  (intercalate)
import           Data.Typeable              (Typeable)
import           Language.JavaScript.Parser (JSNode (..))

data JSCompilerException = ModuleNotFoundException ModuleName
                         | CircularDependencyException [ModuleName]
                         | ParseException [String]
                         | MissingDependencies String
                         -- When the compiler itself is buggy
                         | InternalParserException String
                         -- When language-javascript somehow produces
                         -- a malformed AST
                         | LanguageJavaScriptException JSNode
  deriving (Typeable)

instance Exception JSCompilerException

instance Show JSCompilerException where
    show (ParseException e) =
      "You have some syntax error in your JavaScript:\n" ++ unlines e
    show (CircularDependencyException names) =
      "Your module dependencies form a cycle:\n" ++ intercalate " => " names
    show (ModuleNotFoundException name) =
      "The module \"" ++ name ++ "\" is not found."
    show (InternalParserException e) =
      "Uh oh. The parser itself is misbehaving: " ++ e
    show (LanguageJavaScriptException element) =
      "language-javascript is not parsing the file correctly:\n" ++ show element
    show (MissingDependencies name) =
      "Module `" ++ name ++ "` is missing a dependency array in its `define()`."

type LineNumber = Int
type ModuleName = String
type DepScore = Int
-- A module consists of its name, its dependencies by names, and the node
-- itself.
data Module = Module
            { _name         :: ModuleName
            , _dependencies :: [ModuleName]
            , _node         :: JSNode
            , _score        :: Maybe DepScore
            } deriving (Show)

type OrderedModules = State [Module]
