module Text.Docvim.Compile (compile) where

import Text.Docvim.AST (Node(Project))
import Text.Docvim.Visitor.Command (extractCommand)
import Text.Docvim.Visitor.Commands (extractCommands)
import Text.Docvim.Visitor.Footer (extractFooter)
import Text.Docvim.Visitor.Function (extractFunction)
import Text.Docvim.Visitor.Functions (extractFunctions)
import Text.Docvim.Visitor.Heading (injectTOC)
import Text.Docvim.Visitor.Mapping (extractMapping)
import Text.Docvim.Visitor.Mappings (extractMappings)
import Text.Docvim.Visitor.Option (extractOption)
import Text.Docvim.Visitor.Options (extractOptions)
import Text.Docvim.Visitor.Plugin (extractPlugin)
import Text.Docvim.Visitor.Section ( injectCommands
                                   , injectFunctions
                                   , injectMappings
                                   , injectOptions
                                   )
import Text.Docvim.Visitor (extract)

-- | "Compile" a set of translation units into a project.
compile :: [Node] -> Node
compile ns = do
  let ast = foldr ($) (Project ns) [ injectCommands
                                   , injectFunctions
                                   , injectMappings
                                   , injectOptions
                                   ]
  let (ast2, plugin) = extract extractPlugin ast
  let (ast3, commands) = extract extractCommands ast2
  let (ast4, command) = extract extractCommand ast3
  let (ast5, functions) = extract extractFunctions ast4
  let (ast6, function) = extract extractFunction ast5
  let (ast7, mappings) = extract extractMappings ast6
  let (ast8, mapping) = extract extractMapping ast7
  let (ast9, options) = extract extractOptions ast8
  let (ast10, option) = extract extractOption ast9
  let (ast11, footer) = extract extractFooter ast10
  injectTOC $ Project $ concat [ plugin
                               , [ast11]
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
