{-# LANGUAGE LambdaCase #-}

module Docvim.Visitor.Footer (extract) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad.Trans.Writer
import qualified Data.DList as DList
import Docvim.AST
import qualified Docvim.Visitor as Visitor

-- | Extracts a list of nodes (if any exist) from the `@footer` section(s) of
-- the source code.
--
-- It is not recommended to have multiple footers in a project. If multiple
-- footers (potentially across multiple translation units) exist, there are no
-- guarantees about order but they just get concatenated in the order we see
-- them.
-- extract :: Node -> (Node, [Node])
extract = Visitor.extract extractNodeFooters

-- | Returns True if a node marks the end of a @footer region.
endFooter :: Node -> Bool
endFooter = \case
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

extractFooters :: Alternative f => [Node] -> (f [Node], [Node])
extractFooters = Visitor.extractBlocks f
  where
    f x = if x == FooterAnnotation
          then Just endFooter
          else Nothing
