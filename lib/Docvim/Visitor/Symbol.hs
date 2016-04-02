module Docvim.Visitor.Symbol (getSymbols) where

import Data.Char (toLower)
import Data.List (nub, sort)
import qualified Data.Set as Set
import Docvim.AST

-- TODO: return Set instead of [String]
-- TODO: use Either instead of dying unceremoniously with `error`
getSymbols :: Node -> [String]
getSymbols node = if length symbols == Set.size set
                  then symbols
                  else error $ "Duplicate symbol table entries: " ++ show duplicates
  where
    set                                           = Set.fromList symbols
    symbols                                       = walk gatherSymbols [] node
    gatherSymbols nodes (HeadingAnnotation h)     = mappend nodes [titleAnchor h]
    gatherSymbols nodes (LinkTargets ts)          = mappend nodes ts
    -- TODO: probably don't want this target to exist in the symbol table when
    -- emitting Markdown
    gatherSymbols nodes (PluginAnnotation name _) = mappend nodes [name, name ++ ".txt"]
    gatherSymbols nodes _                         = nodes
    titleAnchor title                             = titlePrefix ++ sanitizeAnchor title
    titlePrefix                                   = downcase $ maybe "" (++ "-") $ getPluginName node
    duplicates                                    = findDupes (sort symbols)
      where
        findDupes [] = []
        findDupes [x] = []
        findDupes (x:xs) = if x == head xs
                           then [x] ++ findDupes xs
                           else findDupes xs

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
