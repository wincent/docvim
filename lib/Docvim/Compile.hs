module Docvim.Compile (compile) where

import Docvim.AST (Node(Project))
import Docvim.Visitor.Footer (extractFooter)
import Docvim.Visitor.Mappings (extractMappings)
import Docvim.Visitor.Plugin (extractPlugin)
import Docvim.Visitor (extract)

-- | "Compile" a set of translation units into a project.
compile :: [Node] -> Node
compile ns = do
  let (ast, footer) = extract extractFooter $ Project ns
  let (ast2, plugin) = extract extractPlugin ast
  let (ast3, mappings) = extract extractMappings ast2
  let project = Project $ concat [plugin, [ast3], mappings, footer]
  project
