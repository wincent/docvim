module Text.Docvim.Visitor.Commands (extractCommands) where

import Control.Applicative
import Text.Docvim.AST
import Text.Docvim.Visitor

-- | Extracts a list of nodes (if any exist) from the `@commands` section(s) of
-- the source code.
--
-- It is not recommended to have multiple `@commands` sections in a project. If
-- multiple such sections (potentially across multiple translation units) exist,
-- there are no guarantees about order; they just get concatenated in the order
-- we see them.
extractCommands :: Alternative f => [Node] -> (f [Node], [Node])
extractCommands = extractBlocks f
  where
    f x = if x == CommandsAnnotation
          then Just endSection
          else Nothing
