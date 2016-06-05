module Docvim.Visitor.Footer (extract) where

import Control.Applicative (Alternative)
import Docvim.AST (Node(FooterAnnotation))
import qualified Docvim.Visitor as Visitor

-- | Extracts a list of nodes (if any exist) from the `@footer` section(s) of
-- the source code.
--
-- It is not recommended to have multiple footers in a project. If multiple
-- footers (potentially across multiple translation units) exist, there are no
-- guarantees about order but they just get concatenated in the order we see
-- them.
extract :: Node -> (Node, [Node])
extract = Visitor.extract extractFooters

extractFooters :: Alternative f => [Node] -> (f [Node], [Node])
extractFooters = Visitor.extractBlocks f
  where
    f x = if x == FooterAnnotation
          then Just Visitor.endBlock
          else Nothing
