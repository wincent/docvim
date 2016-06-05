module Docvim.Visitor.Footer (extractFooter) where

import Control.Applicative (Alternative)
import Docvim.AST (Node(FooterAnnotation))
import Docvim.Visitor (endBlock, extractBlocks)

-- | Extracts a list of nodes (if any exist) from the `@footer` section(s) of
-- the source code.
--
-- It is not recommended to have multiple footers in a project. If multiple
-- footers (potentially across multiple translation units) exist, there are no
-- guarantees about order but they just get concatenated in the order we see
-- them.
extractFooter :: Alternative f => [Node] -> (f [Node], [Node])
extractFooter = extractBlocks f
  where
    f x = if x == FooterAnnotation
          then Just endBlock
          else Nothing
