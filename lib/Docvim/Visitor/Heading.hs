module Docvim.Visitor.Heading ( getHeadings
                              , injectTOC
                              ) where

import Control.Lens
import Control.Lens.Plated (transform)
import Data.Data.Lens (uniplate)
import Docvim.AST

-- | Returns a list of all headings, in the order in which they appear in the
-- AST.
getHeadings :: Node -> [String]
getHeadings = walk gather []
  where
    gather CommandsAnnotation    = ["Commands"]
    gather FunctionsAnnotation   = ["Functions"]
    gather (HeadingAnnotation h) = [h]
    gather MappingsAnnotation    = ["Mappings"]
    gather OptionsAnnotation     = ["Options"]
    gather _                     = []

-- | Injects a table of contents immediately after any `PluginAnnotation` in an
-- AST (note: there should only be one).
-- TODO: warn or error if there is more than one.
-- or use a monadic variant of transform to do only the first...
injectTOC :: Node -> Node
injectTOC ast = transform inject ast
  where
    inject n = case n of
      PluginAnnotation {} -> DocBlock $ n : [TOC $ getHeadings ast]
      _                   -> n
