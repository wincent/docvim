{-# LANGUAGE LambdaCase #-}

module Text.Docvim.Visitor.Option (extractOption) where

import Control.Applicative
import Text.Docvim.AST
import Text.Docvim.Visitor

-- | Extracts a list of nodes (if any exist) identified by the `@option`
-- annotation of the source code.
extractOption :: Alternative f => [Node] -> (f [Node], [Node])
extractOption = extractBlocks f
  where
    f = \case
      OptionAnnotation {} -> Just endSection
      _                   -> Nothing
