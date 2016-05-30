module Docvim.Visitor.Footer (extract) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad ((>=>))
import Control.Monad.Trans.Writer
import qualified Data.DList as DList
import Data.Data.Lens
import Docvim.AST

-- | Extracts a list of nodes (if any exist) from the `@footer` section(s) of
-- the source code.
--
-- It is not recommended to have multiple footers in a project. If multiple
-- footers (potentially across multiple translation units) exist, there are no
-- guarantees about order but they just get concatenated in the order we see
-- them.
extract :: Node -> (Node, [Node])
extract = toList . runWriter . postorder uniplate extractNodeFooters
  where toList (ast, dlist) = (ast, concat $ DList.toList dlist)

-- TODO: consider making this a pipeline, where we just append the footer to the
-- end of the AST after extraction.

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

-- TODO: change the name here; not super happy with it
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
