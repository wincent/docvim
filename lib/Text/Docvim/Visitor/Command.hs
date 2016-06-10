{-# LANGUAGE LambdaCase #-}

module Text.Docvim.Visitor.Command (extractCommand) where

import Control.Applicative
import Text.Docvim.AST
import Text.Docvim.Visitor

-- | Extracts a list of nodes (if any exist) identified by the `@command`
-- annotation of the source code.
extractCommand :: Alternative f => [Node] -> (f [Node], [Node])
extractCommand = extractBlocks f
  where
    f = \case
      CommandAnnotation {} -> Just endSection
      _                    -> Nothing
