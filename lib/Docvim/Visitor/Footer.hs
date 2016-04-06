module Docvim.Visitor.Footer (getFooter) where

import Control.Monad.State
import Debug.Trace
import Docvim.AST

-- | State monad where the state is a Bool indicating whether we are
-- accumulating nodes after a `@footer` annotation, and the result is a list of
-- the accumulated nodes.
type Env = State (Bool, [Node]) [Node]

-- | Gets a list of nodes (if any exist) from the `@footer` section of the
-- source code.
--
-- If multiple footers (potentially across multiple translation units) no
-- guarantees about order but they just get concatenated in the order we see
-- them
getFooter :: Node -> [Node]
getFooter n = evalState (node n) (False, [])

nodes :: [Node] -> Env
nodes ns = concat <$> mapM node ns

node :: Node -> Env
node n = case n of
  CommandAnnotation _ -> do
    (_, acc) <- get
    put (False, acc)
    return $ acc

  DocBlock d -> do
    (_, acc) <- get
    ns <- nodes d
    put (False, acc) -- Make sure we reset state on exiting docblock.
    return $ acc ++ ns

  FooterAnnotation -> do
    (_, acc) <- get
    put (True, acc)
    return $ acc

  MappingAnnotation _ -> do
    (_, acc) <- get
    put (False, acc)
    return $ acc

  MappingsAnnotation -> do
    (_, acc) <- get
    put (False, acc)
    return $ acc

  OptionAnnotation _ _ _ -> do
    (_, acc) <- get
    put (False, acc)
    return $ acc

  PluginAnnotation _ _ -> do
    (_, acc) <- get
    put (False, acc)
    return $ acc

  Unit u -> nodes u

  _ -> do
    (capture, acc) <- get
    return $ if capture
             then acc ++ [n]
             else acc
