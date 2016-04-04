module Docvim.Visitor.Plugin (getPluginName) where

import Data.Monoid ((<>))
import Docvim.AST

-- | Returns the name of the plug-in or Nothing if none is found.
--
-- In the event that there are multiple @plugin annotations competing to define
-- the name of plugin, the first encountered one wins.
getPluginName :: Node -> Maybe String
getPluginName node = name
  where
    name = if null names
           then Nothing
           else Just $ head names
    names = walk getName [] node
    getName nodes (PluginAnnotation name _) = nodes <> [name]
    getName nodes _                         = nodes
