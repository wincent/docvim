{-# LANGUAGE LambdaCase #-}

module Text.Docvim.Visitor.Plugin ( getPluginName
                                  , extractPlugin
                                  ) where

import Control.Applicative (Alternative)
import Text.Docvim.AST (Node(PluginAnnotation), walk)
import Text.Docvim.Visitor (endSection, extractBlocks)

-- | Returns the name of the plug-in or Nothing if none is found.
--
-- In the event that there are multiple `@plugin` annotations competing to
-- define the name of plugin, the first encountered one wins.
getPluginName :: Node -> Maybe String
getPluginName node = name
  where
    name = if null names
           then Nothing
           else Just $ head names
    names = walk getName [] node
    getName (PluginAnnotation name _) = [name]
    getName _                         = []

-- | Extracts a list of nodes (if any exist) from the `@plugin` section(s) of
-- the source code.
--
-- It is not recommended to have multiple `@plugin` sections in a project. If
-- multiple such sections (potentially across multiple translation units) exist,
-- there are no guarantees about order; they just get concatenated in the order
-- we see them.
extractPlugin :: Alternative f => [Node] -> (f [Node], [Node])
extractPlugin = extractBlocks f
  where
    f = \case
      PluginAnnotation {} -> Just endSection
      _                   -> Nothing
