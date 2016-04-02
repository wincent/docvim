module Docvim.Visitor.Symbol (getSymbols) where

import Data.Char (toLower)
import Docvim.AST

-- TODO: error on duplicates; use a real set
getSymbols :: Node -> [String]
getSymbols node = symbols
  where
    symbols                                       = walk gatherSymbols [] node
    gatherSymbols nodes (HeadingAnnotation h)     = mappend nodes [titleAnchor h]
    gatherSymbols nodes (LinkTargets ts)          = mappend nodes ts
    -- TODO: probably don't want this target to exist in the symbol table when
    -- emitting Markdown
    gatherSymbols nodes (PluginAnnotation name _) = mappend nodes [name, name ++ ".txt"]
    gatherSymbols nodes _                         = nodes
    titleAnchor title                             = titlePrefix ++ sanitizeAnchor title
    titlePrefix                                   = downcase $ maybe "" (++ "-") $ getPluginName node

downcase :: String -> String
downcase = map toLower

getPluginName :: Node -> Maybe String
getPluginName node = name
  where
    name = if length names == 0
           then Nothing
           else Just $ head names
    names = walk getName [] node
    getName nodes (PluginAnnotation name _) = mappend nodes [name]
    getName nodes _                         = nodes
