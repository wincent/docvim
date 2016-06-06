module Docvim.Visitor.Options (extractOptions) where

import Control.Applicative (Alternative)
import Docvim.AST (Node(OptionsAnnotation))
import Docvim.Visitor (endBlock, extractBlocks)

-- | Extracts a list of nodes (if any exist) from the `@options` section(s) of
-- the source code.
--
-- It is not recommended to have multiple `@options` sections in a project. If
-- multiple such sections (potentially across multiple translation units) exist,
-- there are no guarantees about order; they just get concatenated in the order
-- we see them.
extractOptions :: Alternative f => [Node] -> (f [Node], [Node])
extractOptions = extractBlocks f
  where
    f x = if x == OptionsAnnotation
          then Just endBlock
          else Nothing