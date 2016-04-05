module Docvim.Visitor.Symbol (getSymbols) where

import Data.Char (toLower)
import Data.List (nub, sort)
import qualified Data.Set as Set
import Docvim.AST
import Docvim.Visitor.Plugin (getPluginName)

-- TODO: return Set instead of [String]
-- TODO: use Either instead of dying unceremoniously with `error`
getSymbols :: Node -> [String]
getSymbols node = if length symbols == Set.size set
                  then symbols
                  else error $ "Duplicate symbol table entries: " ++ show duplicates
  where
    set                                     = Set.fromList symbols
    symbols                                 = walk gatherSymbols [] node
    gatherSymbols (HeadingAnnotation h)     = [titleAnchor h]
    gatherSymbols (LinkTargets ts)          = ts
    -- TODO: probably don't want this target to exist in the symbol table when
    -- emitting Markdown
    gatherSymbols (PluginAnnotation name _) = [name, name ++ ".txt"]
    gatherSymbols _                         = []
    titleAnchor title                       = titlePrefix ++ sanitizeAnchor title
    titlePrefix                             = downcase $ maybe "" (++ "-") $ getPluginName node
    duplicates                              = nub $ f (sort symbols)
      where
        f [] = []
        f [x] = []
        f (x:xs) = if x == head xs
                   then x : f xs
                   else f xs

downcase :: String -> String
downcase = map toLower
