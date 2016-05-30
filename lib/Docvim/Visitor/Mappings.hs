module Docvim.Visitor.Mappings (extract) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad ((>=>))
import Control.Monad.Trans.Writer
import qualified Data.DList as DList
import Data.Data.Lens
import Docvim.AST

-- | Extracts a list of nodes (if any exist) from the `@mappings` section(s) of
-- the source code.
--
-- It is not recommended to have multiple `@mappings` sections in a project. If
-- multiple such sections (potentially across multiple translation units) exist,
-- there are no guarantees about order; they just get concatenated in the order
-- we see them.
extract :: Node -> (Node, [Node])
extract = toList . runWriter . postorder uniplate extractNodeMappings
  where toList (ast, dlist) = (ast, concat $ DList.toList dlist)

-- | Returns True if a node marks the end of a mappings region.
endMappings :: Node -> Bool
endMappings n = case n of
  CommandAnnotation _ -> True
  FooterAnnotation       -> True
  FunctionAnnotation _   -> True
  MappingAnnotation _    -> True
  MappingsAnnotation     -> True
  OptionAnnotation {}    -> True
  PluginAnnotation {}    -> True
  _                      -> False

extractNodeMappings :: Node -> Writer (DList.DList [Node]) Node
extractNodeMappings (DocBlock nodes) = do
    let (plugins, remainder) = extractMappings nodes
    tell (DList.fromList plugins)
    return (DocBlock remainder)
extractNodeMappings node = return node

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
-- TODO factor this out, probably moving it into the AST module

extractMappings :: Alternative f => [Node] -> (f [Node], [Node])
extractMappings = extractBlocks f
  where
    f x = if x == MappingsAnnotation
          then Just endMappings
          else Nothing

postorder :: Monad m => ((a -> m c) -> (a -> m b)) -> (b -> m c) -> (a -> m c)
postorder t f = go
  where
    go = t go >=> f
