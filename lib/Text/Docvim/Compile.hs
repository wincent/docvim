module Text.Docvim.Compile (compile) where

import Text.Docvim.AST
import Text.Docvim.Visitor
import Text.Docvim.Visitor.Command
import Text.Docvim.Visitor.Commands
import Text.Docvim.Visitor.Footer
import Text.Docvim.Visitor.Function
import Text.Docvim.Visitor.Functions
import Text.Docvim.Visitor.Heading
import Text.Docvim.Visitor.Mapping
import Text.Docvim.Visitor.Mappings
import Text.Docvim.Visitor.Option
import Text.Docvim.Visitor.Options
import Text.Docvim.Visitor.Plugin
import Text.Docvim.Visitor.Section

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
