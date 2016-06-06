module Docvim.Compile (compile) where

import Docvim.AST (Node(Project))
import Docvim.Visitor.Commands (extractCommands)
import Docvim.Visitor.Footer (extractFooter)
import Docvim.Visitor.Functions (extractFunctions)
import Docvim.Visitor.Mappings (extractMappings)
import Docvim.Visitor.Options (extractOptions)
import Docvim.Visitor.Plugin (extractPlugin)
import Docvim.Visitor (extract)

-- | "Compile" a set of translation units into a project.
compile :: [Node] -> Node
compile ns = do
  let (ast, footer) = extract extractFooter $ Project ns
  let (ast2, plugin) = extract extractPlugin ast
  let (ast3, commands) = extract extractCommands ast2
  let (ast4, functions) = extract extractFunctions ast3
  let (ast5, mappings) = extract extractMappings ast4
  let (ast6, options) = extract extractOptions ast5
  let project = Project $ concat [ plugin
                                 , [ast6]
                                 , commands
                                 , mappings
                                 , options
                                 , functions
                                 , footer
                                 ]
  project
