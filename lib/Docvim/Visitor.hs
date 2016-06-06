{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Docvim.Visitor (endSection, extract, extractBlocks) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad ((>=>))
import Control.Monad.Writer (runWriter, tell)
import Data.Data.Lens
import Docvim.AST
import qualified Data.DList as DList

-- | Returns True if a node marks the end of a region/block/section.
endSection :: Node -> Bool
endSection = \case
  CommandAnnotation _    -> True
  CommandsAnnotation     -> True
  FooterAnnotation       -> True
  FunctionAnnotation _   -> True
  FunctionsAnnotation    -> True
  MappingAnnotation _    -> True
  MappingsAnnotation     -> True
  OptionAnnotation {}    -> True
  OptionsAnnotation      -> True
  PluginAnnotation {}    -> True
  _                      -> False

extract :: ([Node] -> ([[a]], [Node])) -> Node -> (Node, [a])
extract extractNodes = toList . runWriter . postorder uniplate extractor
  where
    toList (ast, dlist) = (ast, concat $ DList.toList dlist)
    extractor (DocBlock nodes) = do
      let (extracted, remainder) = extractNodes nodes
      tell (DList.fromList extracted)
      return (DocBlock remainder)
    extractor node = return node

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

postorder :: Monad m => ((a -> m c) -> (a -> m b)) -> (b -> m c) -> (a -> m c)
postorder t f = go
  where
    go = t go >=> f
