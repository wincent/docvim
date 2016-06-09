{-# LANGUAGE LambdaCase #-}

module Text.Docvim.Visitor.Command (extractCommand) where

import Control.Applicative (Alternative)
import Text.Docvim.AST (Node(CommandAnnotation))
import Text.Docvim.Visitor (endSection, extractBlocks)

-- | Extracts a list of nodes (if any exist) identified by the `@command`
-- annotation of the source code.
extractCommand :: Alternative f => [Node] -> (f [Node], [Node])
extractCommand = extractBlocks f
  where
    f = \case
      CommandAnnotation {} -> Just endSection
      _                    -> Nothing
