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
    symbols = linkTargets

    -- TODO: merge these into a single walk
    headings = concat $ walk gatherHeadings [] node
    gatherHeadings nodes (HeadingAnnotation h) = mappend nodes [h]
    gatherHeadings nodes _ = nodes

    linkTargets = concat $ walk gatherLinkTargets [] node
    gatherLinkTargets nodes (LinkTargets ts) = mappend nodes [ts]
    gatherLinkTargets nodes _ = nodes

    pluginAnnotations = concat $ walk gatherPluginAnnotations [] node
    gatherPluginAnnotations nodes (PluginAnnotation name _) = mappend nodes [name]
    gatherPluginAnnotations nodes _ = nodes
