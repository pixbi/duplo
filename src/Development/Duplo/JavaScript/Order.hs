{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.JavaScript.Order where

import           Control.Applicative                ((<$>))
import           Control.Exception                  (throw)
import           Control.Lens                       (ix, makeLenses)
import           Control.Lens.Operators
import           Control.Monad                      (liftM, void, when)
import           Control.Monad.State.Lazy           (execState, get, put, state)
import           Control.Monad.Writer.Lazy          (Writer, runWriter, tell)
import           Data.Function                      (on)
import           Data.List                          (findIndex, nubBy, sortBy)
import           Data.Maybe                         (fromJust, fromMaybe,
                                                     isJust)
import           Development.Duplo.Types.JavaScript
import           Language.JavaScript.Parser         (JSNode (..), Node (..),
                                                     TokenPosn (..))

makeLenses ''Module

-- | Reorder modules within the root node.
order :: JSNode -> JSNode
order jsNode =
    -- Append the modules, in its rightful order, to all the non-applicable
    -- nodes.
    NN $ JSSourceElementsTop $ naNodes ++ aNodesWithSep
  where
    -- For hygiene
    separator = NT (JSLiteral ";") (TokenPn 0 0 0) []
    -- The "normal" output is the filtered non-applicable nodes and the
    -- written-to nodes are of course the extracted ones.
    (naNodes, aNodes) = runWriter $ extract jsNode
    -- Reorder modules
    orderedANodes = _node <$> reorder aNodes
    -- Insert separators
    aNodesWithSep = concat $ fmap (\n -> [n, separator]) orderedANodes

-- | Extract AMD modules to logger for re-ordering and the rest to output.
extract :: JSNode -> Writer [Module] [JSNode]
-- Go through all elements at top-level.
extract (NN (JSSourceElementsTop jsElements)) = mapM extract' jsElements
-- Impossible scenario
extract element = throw $ LanguageJavaScriptException element

extract' :: JSNode -> Writer [Module] JSNode
-- We are looking for a `define`!
extract' jsNode@(NN (JSExpression (NT (JSIdentifier "define") _ _:args:_))) = do
  -- Save the module for processing.
  tell [makeModule jsNode args]
  -- Just return an empty string in place of the module.
  return $ NT (JSIdentifier "") (TokenPn 0 0 0) []
-- Just pass everything else through.
extract' jsNode = return jsNode

-- | Turn a node into a module.
           -- The root expression node
makeModule :: JSNode
           -- The argument node
           -> JSNode
           -- The created module
           -> Module
makeModule rootNode argNode =
    Module moduleName deps rootNode Nothing
  where
    -- Extract the terminating nodes that are arguments.
    (NN (JSArguments _ argNTs _)) = argNode
    -- A simple pattern matching for the module declaration works, it
    -- always follows this pattern.
    (nameNT:_:depsNT:_) = argNTs
    -- Extract the name, always first argument.
    (NT (JSStringLiteral _ moduleName) _ _) = nameNT
    -- Extract the module dependencies. The dependency list is always the
    -- third argument; otherwise, there's an issue.
    (NN (JSArrayLiteral _ depsNodes _)) = matchDependencies moduleName depsNT
    -- Only take the strings.
    deps = map fromJust $ filter isJust $ map stringLiteralNT depsNodes

matchDependencies :: String -> JSNode -> JSNode
matchDependencies _ node@(NN (JSArrayLiteral _ depsNodes _)) = node
matchDependencies moduleName _ = throw $ MissingDependencies moduleName

-- | Given a JSNode, return just the string literal, or nothing
stringLiteralNT :: JSNode -> Maybe String
stringLiteralNT (NT (JSStringLiteral _ string) _ _) = Just string
stringLiteralNT _ = Nothing

-- | Reorder all the applicable modules
reorder :: [Module] -> [Module]
reorder mods = nubbed
  where
    -- Score all the modules
    scored = execState computeScores mods
    -- Filter out initial modules that may have a score of `Nothing`
    filtered = filter withScore scored
    -- Sort by score
    sorted = sortBy byDepScore filtered
    -- Deduplicate, keeping the higher score ones
    nubbed = reverse $ nubBy ((==) `on` _name) $ reverse sorted

withScore :: Module -> Bool
withScore aMod = case _score aMod of
                   Just _ -> True
                   Nothing -> False

byDepScore :: Module -> Module -> Ordering
byDepScore a b = compare (_score a) (_score b)

-- | Given a module list, find all the dependency scores of the constituent
-- modules.
computeScores :: OrderedModules [DepScore]
computeScores = do
    mods <- get
    -- Put the modules in state to be re-ordered as well as extract the
    -- names by placing it in the State as values.
    mapM (getDepScore []) $ fmap _name mods

-- | Given a module name, get its score.
getDepScore :: [ModuleName] -> ModuleName -> OrderedModules DepScore
getDepScore history modName = do
    -- We go nuclear when there's circular dependency.
    let history' = modName : history
    -- Display the duplicate module if it's in the recorded modules
    void $ when (modName `elem` history)
         $ throw $ CircularDependencyException $ reverse history'
    -- Take out the modules.
    mods <- get
    -- Assume module exist, as it should at this point.
    let maybeIndex = findIndex ((modName ==) . _name) mods
    let index = fromMaybe (throw $ ModuleNotFoundException modName) maybeIndex
    -- Get the actual module.
    let aMod = fromJust $ mods ^? ix index
    -- The dependency list
    let modDeps = _dependencies aMod
    -- If there is a score, use it; otherwise, obviously go get it.
    depScore <- case _score aMod of
                  -- Re-wrap with the score as the result.
                  Just modScore -> state $ const (modScore, mods)
                  -- Go through the dependencies' individual scores.
                  Nothing -> getDepScore' history' modDeps
    -- Update the score.
    let newMod = aMod & score .~ Just depScore
    -- TODO: somehow can't get Lens to work. Doing the old fashion way.
    let newMods = take index mods ++ [newMod] ++ drop (index + 1) mods
    put newMods
    -- Return the sscore.
    return depScore

-- | Get a module's dependency score given its dependencies.
getDepScore' :: [ModuleName] -> [ModuleName] -> OrderedModules DepScore
getDepScore' history modNames =
    liftM ((1 +) . sum) $ mapM (getDepScore history) modNames
