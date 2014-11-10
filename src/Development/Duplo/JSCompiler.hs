module Development.Duplo.JSCompiler
  ( compile
  ) where

import Language.JavaScript.Parser (JSNode(..), Node(..), TokenPosn(..))
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)

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
    aNodes' = fmap runModule $ reorder aNodes

-- | Extract AMD modules to logger for re-ordering and the rest to output.
extract :: JSNode -> Writer [Module] [JSNode]
-- Go through all elements at top-level.
extract node@(NN (JSSourceElementsTop elements)) = mapM extract' elements

extract' :: JSNode -> Writer [Module] JSNode
-- We are looking for a `define`!
extract' node@(NN (JSExpression ((NT (JSIdentifier "define") _ _):_))) = do
  -- Save the module for processing.
  tell $ [makeModule node]
  -- Just return an empty string in place of the module.
  return $ NT (JSIdentifier "") (TokenPn 0 0 0) []
-- Just pass everything else through.
extract' node = return node

-- | Turn a node into a module.
makeModule :: JSNode -> Module
makeModule = undefined

-- | Turn a module into a node
runModule :: Module -> JSNode
runModule = undefined

-- | Reorder all the applicable modules
reorder :: [Module] -> [Module]
reorder = undefined
