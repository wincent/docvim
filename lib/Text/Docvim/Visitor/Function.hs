{-# LANGUAGE LambdaCase #-}

module Text.Docvim.Visitor.Function (extractFunction) where

import Control.Applicative (Alternative)
import Text.Docvim.AST (Node(FunctionAnnotation))
import Text.Docvim.Visitor (endSection, extractBlocks)

-- | Extracts a list of nodes (if any exist) identified by the `@function`
-- annotation of the source code.
extractFunction :: Alternative f => [Node] -> (f [Node], [Node])
extractFunction = extractBlocks f
  where
    f = \case
      FunctionAnnotation _ -> Just endSection
      _                    -> Nothing
-- TODO: plug this in to the Compile module
-- (note will need to implement printing as well)
-- TODO: verify that these do what we think they should do... (should they wrap
-- what they return in an array, or do they all get merged, and if they do, is
-- that ok? it probably is)
-- TODO: DRY these visitors up; they are all almost exactly the same
