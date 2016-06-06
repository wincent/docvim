module Docvim.Compile (compile) where

import Docvim.AST (Node(Project))
import Docvim.Visitor.Command (extractCommand)
import Docvim.Visitor.Commands (extractCommands)
import Docvim.Visitor.Footer (extractFooter)
import Docvim.Visitor.Function (extractFunction)
import Docvim.Visitor.Functions (extractFunctions)
import Docvim.Visitor.Mapping (extractMapping)
import Docvim.Visitor.Mappings (extractMappings)
import Docvim.Visitor.Option (extractOption)
import Docvim.Visitor.Options (extractOptions)
import Docvim.Visitor.Plugin (extractPlugin)
import Docvim.Visitor (extract)

-- | "Compile" a set of translation units into a project.
compile :: [Node] -> Node
compile ns = do
  let (ast, footer) = extract extractFooter $ Project ns
  let (ast2, plugin) = extract extractPlugin ast
  let (ast3, commands) = extract extractCommands ast2
  let (ast4, command) = extract extractCommand ast3
  let (ast5, functions) = extract extractFunctions ast4
  let (ast6, function) = extract extractFunction ast5
  let (ast7, mappings) = extract extractMappings ast6
  let (ast8, mapping) = extract extractMapping ast7
  let (ast9, options) = extract extractOptions ast8
  let (ast10, option) = extract extractOption ast9
  let project = Project $ concat [ plugin
                                 , [ast10]
                                 , commands
                                 , command
                                 , mappings
                                 , mapping
                                 , options
                                 , option
                                 , functions
                                 , function
                                 , footer
                                 ]
  project
