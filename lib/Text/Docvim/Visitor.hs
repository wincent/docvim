{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Docvim.Visitor (endSection, extract, extractBlocks) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Data.Lens
import Text.Docvim.AST
import qualified Data.DList as DList

-- | Returns True if a node marks the end of a region/block/section.
endSection :: Node -> Bool
endSection = \case
  CommandAnnotation {}   -> True
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
    go (x:xs) = maybe noExtract extract' (start x)
      where
        noExtract = (extracted, x:unextracted)
          where
            ~(extracted, unextracted) = go xs
        extract' stop = (pure (x:block) <|> extracted, unextracted)
          where
            ~(block, remainder) = break stop xs
            ~(extracted, unextracted) = go remainder

postorder :: Monad m => ((a -> m c) -> a -> m b) -> (b -> m c) -> a -> m c
postorder t f = go
  where
    go = t go >=> f
