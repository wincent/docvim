module Docvim.Visitor.Symbol (getSymbols) where

import Docvim.AST

getSymbols :: Node -> [String]
getSymbols node = symbols
  where
    -- TODO: error on duplicates; use a real set
    symbols = concat $ walk gatherSymbol [] node
    gatherSymbol nodes (LinkTargets ts) = mappend nodes [ts]
    gatherSymbol nodes _ = nodes
