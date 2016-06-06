{-# LANGUAGE LambdaCase #-}

module Docvim.Visitor.Option (extractOption) where

import Control.Applicative (Alternative)
import Docvim.AST (Node(OptionAnnotation))
import Docvim.Visitor (endSection, extractBlocks)

-- | Extracts a list of nodes (if any exist) identified by the `@option`
-- annotation of the source code.
extractOption :: Alternative f => [Node] -> (f [Node], [Node])
extractOption = extractBlocks f
  where
    f = \case
      OptionAnnotation {} -> Just endSection
      _                   -> Nothing
