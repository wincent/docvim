module Docvim.Visitor.Symbol (getSymbols) where

import Control.Monad (join)
import Docvim.AST

-- TODO: add auto-gened symbols too:
--      @plugin foo bar     -> foo and foo.txt
--      # Some heading      -> some-heading
-- (will extract and re-use the sanitizeAnchor logic from the Markdown printer
getSymbols :: Node -> [String]
getSymbols node = symbols
  where
    -- TODO: error on duplicates; use a real set
    symbols                                       = walk gatherSymbols [] node
    gatherSymbols nodes (HeadingAnnotation h)     = mappend nodes [sanitizeAnchor h]
    gatherSymbols nodes (LinkTargets ts)          = mappend nodes (join [ts])
    -- TODO: probably don't want this target to exist in the symbol table when
    -- emitting Markdown
    gatherSymbols nodes (PluginAnnotation name _) = mappend nodes [name, name ++ ".txt"]
    gatherSymbols nodes _                         = nodes
