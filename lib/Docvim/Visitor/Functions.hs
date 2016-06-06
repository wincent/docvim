module Docvim.Visitor.Functions (extractFunctions) where

import Control.Applicative (Alternative)
import Docvim.AST (Node(FunctionsAnnotation))
import Docvim.Visitor (endBlock, extractBlocks)

-- | Extracts a list of nodes (if any exist) from the `@functions` section(s) of
-- the source code.
--
-- It is not recommended to have multiple `@functions` sections in a project. If
-- multiple such sections (potentially across multiple translation units) exist,
-- there are no guarantees about order; they just get concatenated in the order
-- we see them.
extractFunctions :: Alternative f => [Node] -> (f [Node], [Node])
extractFunctions = extractBlocks f
  where
    f x = if x == FunctionsAnnotation
          then Just endBlock
          else Nothing