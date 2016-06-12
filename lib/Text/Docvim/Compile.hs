module Text.Docvim.Compile (compile) where

import Text.Docvim.AST
import Text.Docvim.Optimize
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
    let steps = [ extract extractPlugin
                , extract extractCommands
                , extract extractCommand
                , extract extractMappings
                , extract extractMapping
                , extract extractOptions
                , extract extractOption
                , extract extractFunctions
                , extract extractFunction
                , extract extractFooter
                ]
    let (remainder, sections) = foldl reduce (ast, []) steps
    let (beginning, rest) = splitAt 1 sections
    optimize $ injectTOC $ Project $ (concat . concat) [beginning, [[remainder]], rest]
  where
    reduce (remainder', sections') step = do
      let (r', s') = step remainder'
      (r', sections' ++ [s'])
