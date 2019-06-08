module Text.Docvim.Visitor.Header (extractHeader) where

import Control.Applicative
import Text.Docvim.AST
import Text.Docvim.Visitor

-- | Extracts a list of nodes (if any exist) from the `@header` section(s) of
-- the source code.
--
-- It is not recommended to have multiple headers in a project. If multiple
-- headers (potentially across multiple translation units) exist, there are no
-- guarantees about order but they just get concatenated in the order we see
-- them.
extractHeader :: Alternative f => [Node] -> (f [Node], [Node])
extractHeader = extractBlocks f
  where
    f x = if x == HeaderAnnotation
          then Just endSection
          else Nothing
