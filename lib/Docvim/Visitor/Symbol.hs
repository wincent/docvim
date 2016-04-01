module Docvim.Visitor.Symbol (getSymbols) where

import Docvim.AST

-- TODO: add auto-gened symbols too:
--      @plugin foo bar     -> foo and foo.txt
--      # Some heading      -> some-heading
-- (will extract and re-use the sanitizeAnchor logic from the Markdown printer
getSymbols :: Node -> [String]
getSymbols node = symbols
  where
    -- TODO: error on duplicates; use a real set
    symbols = pluginAnnotations ++ linkTargets ++ headings

    -- TODO: merge these into a single walk
    headings = walk gatherHeadings [] node
    gatherHeadings nodes (HeadingAnnotation h) = mappend nodes [sanitizeAnchor h]
    gatherHeadings nodes _ = nodes

    linkTargets = concat $ walk gatherLinkTargets [] node
    gatherLinkTargets nodes (LinkTargets ts) = mappend nodes [ts]
    gatherLinkTargets nodes _ = nodes

    -- TODO: probably don't want this target to exist in the symbol table when
    -- emitting Markdown
    pluginAnnotations = walk gatherPluginAnnotations [] node
    gatherPluginAnnotations nodes (PluginAnnotation name _) = mappend nodes [name, name ++ ".txt"]
    gatherPluginAnnotations nodes _ = nodes
