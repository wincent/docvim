{-# LANGUAGE LambdaCase #-}

module Docvim.Visitor.Mapping (extractMapping) where

import Control.Applicative (Alternative)
import Docvim.AST (Node(MappingAnnotation))
import Docvim.Visitor (endSection, extractBlocks)

-- | Extracts a list of nodes (if any exist) identified by the `@mapping`
-- annotation of the source code.
extractMapping :: Alternative f => [Node] -> (f [Node], [Node])
extractMapping = extractBlocks f
  where
    f = \case
      MappingAnnotation _ -> Just endSection
      _                   -> Nothing
