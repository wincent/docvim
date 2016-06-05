module Docvim.Visitor (extract, extractBlocks) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad ((>=>))
-- TODO switch to pure mtl here (reduce dependency footprint)
import Control.Monad.Trans.Writer (runWriter)
import Data.Data.Lens
import Docvim.AST (Node)
import qualified Data.DList as DList

extract extractor = toList . runWriter . postorder uniplate extractor
  where toList (ast, dlist) = (ast, concat $ DList.toList dlist)

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
