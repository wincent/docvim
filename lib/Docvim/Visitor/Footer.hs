module Docvim.Visitor.Footer (partitionFooter) where

import Control.Monad.State
import Debug.Trace
import Docvim.AST

-- | State monad where the state is a Bool indicating whether we are
-- accumulating nodes after a `@footer` annotation, and the result is a tuple
-- containing a pair of partitioned lists for the accumulated nodes. In this
-- case, the lists containg the `@footer` nodes and the non-`@footer` nodes
-- respectively.
type Env = State (Bool, [Node], [Node]) ([Node], [Node])

-- | Gets a list of nodes (if any exist) from the `@footer` section of the
-- source code.
--
-- If multiple footers (potentially across multiple translation units) no
-- guarantees about order but they just get concatenated in the order we see
-- them
partitionFooter :: Node -> ([Node], [Node])
partitionFooter n = evalState (node n) (False, [], [])

nodes :: [Node] -> Env
nodes ns = do
  (_, footer, other) <- get
  mapped <- mapM node ns
  let (x, y) = if null mapped then ([],[]) else last mapped
  return (footer ++ x, other ++ y)

setCapturing :: Bool -> Env
setCapturing b = do
  (_, footer, other) <- get
  put (b, footer, other)
  return $ (footer, other)

start :: Env
start = setCapturing True

stop :: Env
stop = setCapturing False

node :: Node -> Env
node n = case n of
  CommandAnnotation _    -> stop
  DocBlock d             -> do
    (_, footer, other)   <- get
    (footer, other)      <- nodes d
    put (False, footer, other)     -- Make sure we reset state on exiting docblock.
    return $ (footer, other)
  FooterAnnotation       -> start
  MappingAnnotation _    -> stop
  MappingsAnnotation     -> stop
  OptionAnnotation _ _ _ -> stop
  PluginAnnotation _ _   -> stop
  Unit u                 -> nodes u
  _                      -> do
    (capture, footer, other) <- get
    return $ if capture
             then (footer ++ [n], other)
             else (footer, other ++ [n])
