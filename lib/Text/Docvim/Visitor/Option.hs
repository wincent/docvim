{-# LANGUAGE LambdaCase #-}

module Text.Docvim.Visitor.Option (extractOption) where

import Control.Applicative (Alternative)
import Text.Docvim.AST (Node(OptionAnnotation))
import Text.Docvim.Visitor (endSection, extractBlocks)

-- | Extracts a list of nodes (if any exist) identified by the `@option`
-- annotation of the source code.
extractOption :: Alternative f => [Node] -> (f [Node], [Node])
extractOption = extractBlocks f
  where
    f = \case
      OptionAnnotation {} -> Just endSection
      _                   -> Nothing
