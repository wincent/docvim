{-# LANGUAGE LambdaCase #-}

module Text.Docvim.Visitor.Image (extractImage) where

import Control.Applicative
import Text.Docvim.AST
import Text.Docvim.Visitor

-- | Extracts a list of nodes (if any exist) identified by the `@image`
-- annotation of the source code.
extractImage :: Alternative f => [Node] -> (f [Node], [Node])
extractImage = extractBlocks f
  where
    f = \case
      ImageAnnotation {} -> Just endSection
      _                  -> Nothing
