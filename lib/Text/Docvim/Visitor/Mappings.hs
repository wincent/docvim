module Text.Docvim.Visitor.Mappings (extractMappings) where

import Control.Applicative (Alternative)
import Text.Docvim.AST (Node(MappingsAnnotation))
import Text.Docvim.Visitor (endSection, extractBlocks)

-- | Extracts a list of nodes (if any exist) from the `@mappings` section(s) of
-- the source code.
--
-- It is not recommended to have multiple `@mappings` sections in a project. If
-- multiple such sections (potentially across multiple translation units) exist,
-- there are no guarantees about order; they just get concatenated in the order
-- we see them.
extractMappings :: Alternative f => [Node] -> (f [Node], [Node])
extractMappings = extractBlocks f
  where
    f x = if x == MappingsAnnotation
          then Just endSection
          else Nothing
-- TODO: DRY all these up
