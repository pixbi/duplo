{-# LANGUAGE DeriveDataTypeable #-}

module Development.Duplo.Types.JavaScript
  ( JSCompilerException(..)
  , ModuleName
  , DepScore
  , Module(..)
  , OrderedModules
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Control.Monad.State.Lazy (State)
import Language.JavaScript.Parser (JSNode(..))

data JSCompilerException = ModuleNotFoundException ModuleName
                         | CircularDependencyException [ModuleName]
                         | ParseException String
  deriving (Show, Typeable)
instance Exception JSCompilerException

type ModuleName = String
type DepScore = Int
-- A module consists of its name, its dependencies by names, and the node
-- itself.
data Module = Module { _name :: ModuleName
                     , _deps :: [ModuleName]
                     , _node :: JSNode
                     , _score :: Maybe DepScore
                     } deriving (Show)

type OrderedModules = State [Module]
