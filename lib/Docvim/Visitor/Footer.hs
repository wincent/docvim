module Docvim.Visitor.Footer ( getFooter
                             , example
                             , extractNodeFooters
                             , postorder
                             ) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad ((>=>))
import Control.Monad.State
import Control.Monad.Trans.Writer
import qualified Data.DList as DList
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
  Project p            -> nodes p
  Unit u               -> nodes u
  _                    -> do
    (capture, acc)     <- get
    return $ if capture
             then acc ++ [n]
             else acc

-- | Returns True if a node marks the end of a @footer region.
endFooter :: Node -> Bool
endFooter n = case n of
    CommandAnnotation _    -> True
    FooterAnnotation       -> True
    FunctionAnnotation _   -> True
    MappingAnnotation _    -> True
    MappingsAnnotation     -> True
    OptionAnnotation {}    -> True
    PluginAnnotation {}    -> True
    _                      -> False

extractNodeFooters :: Node -> Writer (DList.DList [Node]) Node
extractNodeFooters (DocBlock nodes) = do
    let (footers, remainder) = extractFooters nodes
    tell (DList.fromList footers)
    return (DocBlock remainder)
extractNodeFooters node = return node

extractBlocks :: Alternative f => (a -> Maybe (a -> Bool)) -> [a] -> (f [a], [a])
extractBlocks start = go
  where
    go     [] = (empty, [])
    go (x:xs) = maybe no_extract extract (start x)
      where
        no_extract = (extracted, x:unextracted)
          where
            ~(extracted, unextracted) = go xs
        extract stop = (pure (x:block) <|> extracted, unextracted)
          where
            ~(block, remainder) = break stop xs
            ~(extracted, unextracted) = go remainder

extractFooters :: Alternative f => [Node] -> (f [Node], [Node])
extractFooters = extractBlocks f
  where
    f x = if x == FooterAnnotation
          then Just endFooter
          else Nothing

postorder :: Monad m => ((a -> m c) -> (a -> m b)) -> (b -> m c) -> (a -> m c)
postorder t f = go
  where
    go = t go >=> f

preorder :: Monad m => ((a -> m c) -> (b -> m c)) -> (a -> m b) -> (a -> m c)
preorder t f = go
  where
    go = f >=> t go


example = Project [
  Unit [
    Code "Unit Code",
    DocBlock [
      Code "DocBlock Code",
      DocBlock [
        Code "DocBlock DocBlock Code",
        FooterAnnotation,
        Code "DocBlock DocBlock FooterAnnotation Code"
      ],
      FooterAnnotation,
      Code "DocBlock FooterAnnotation Code",
      DocBlock [
        Code "DocBlock FooterAnnotation DocBlock Code",
        FooterAnnotation,
        Code "DocBlock FooterAnnotation DocBlock FooterAnnotation Code"
      ]
    ],
    FooterAnnotation,
    Code "Unit FooterAnnotation Code"] ]

-- Test this out with:
-- import Data.Data.Lens
-- print . postorder uniplate extractNodeFooters $ example
