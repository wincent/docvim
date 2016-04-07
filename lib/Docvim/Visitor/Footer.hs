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

setCapturing :: Bool -> Env
setCapturing b = do
  (_, acc) <- get
  put (b, acc)
  return acc

start :: Env
start = setCapturing True

stop :: Env
stop = setCapturing False

node :: Node -> Env
node n = case n of
  CommandAnnotation _  -> stop
  DocBlock d           -> do
    (_, acc)           <- get
    ns                 <- nodes d
    put (False, acc)   -- Make sure we reset state on exiting docblock.
    return $ acc ++ ns
  FooterAnnotation     -> start
  MappingAnnotation _  -> stop
  MappingsAnnotation   -> stop
  OptionAnnotation {}  -> stop
  PluginAnnotation {}  -> stop
  Unit u               -> nodes u
  _                    -> do
    (capture, acc)     <- get
    return $ if capture
             then acc ++ [n]
             else acc
