module Development.Duplo.JSCompiler
  ( compile
  ) where

import Language.JavaScript.Parser (JSNode(..), Node(..), TokenPosn(..))
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import Data.Maybe (isJust, fromJust)
import Debug.Trace (trace)

type ModuleName = String
-- A module consists of its name, its dependencies by names, and the node
-- itself.
type Module = (ModuleName, [ModuleName], JSNode)

-- | Compile a JSNode.
compile :: JSNode -> JSNode
compile node =
    -- Append the modules, in its rightful order, to all the non-applicable
    -- nodes.
    NN $ JSSourceElementsTop $ naNodes ++ aNodes'
  where
    -- The "normal" output is the filtered non-applicable nodes and the
    -- written-to nodes are of course the extracted ones.
    (naNodes, aNodes) = runWriter $ extract node
    -- Reorder modules
    aNodes' = fmap runModule aNodes

-- | Extract AMD modules to logger for re-ordering and the rest to output.
extract :: JSNode -> Writer [Module] [JSNode]
-- Go through all elements at top-level.
extract node@(NN (JSSourceElementsTop elements)) = mapM extract' elements

extract' :: JSNode -> Writer [Module] JSNode
-- We are looking for a `define`!
extract' node@(NN (JSExpression ((NT (JSIdentifier "define") _ _):args:_))) = do
  -- Save the module for processing.
  tell $ [makeModule node args]
  -- Just return an empty string in place of the module.
  return $ NT (JSIdentifier "") (TokenPn 0 0 0) []
-- Just pass everything else through.
extract' node = return node

-- | Turn a node into a module.
           -- The root expression node
makeModule :: JSNode
           -- The argument node
           -> JSNode
           -- The created module
           -> Module
makeModule rootNode argNode =
    (moduleName, deps, rootNode)
  where
    -- Extract the terminating nodes that are arguments.
    (NN (JSArguments _ argNTs _)) = argNode
    -- A simple pattern matching for the module declaration works, it
    -- always follows this pattern.
    (nameNT:_:depsNT:_) = argNTs
    -- Extract the name, always first argument.
    (NT (JSStringLiteral _ moduleName) _ _) = nameNT
    -- Extract the module dependencies, always the third argument (second
    -- being the comma separator).
    (NN (JSArrayLiteral _ depsNodes _)) = depsNT
    -- Only take the strings.
    deps = map fromJust $ filter isJust $ map stringLiteralNT depsNodes

-- | Given a JSNode, return just the string literal, or nothing
stringLiteralNT :: JSNode -> Maybe String
stringLiteralNT (NT (JSStringLiteral _ string) _ _) = Just string
stringLiteralNT _ = Nothing

-- | Turn a module into a node
runModule :: Module -> JSNode
runModule (name, deps, node) = node

-- | Reorder all the applicable modules
reorder :: [Module] -> [Module]
reorder = undefined
