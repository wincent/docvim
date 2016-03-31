module Docvim.AST where

import Data.List (intercalate)

data Node
          -- Root (translation unit)
          = Unit [Node]

          -- VimL nodes
          | FunctionDeclaration { functionBang :: Bool
                                , functionName :: String
                                , functionArguments :: ArgumentList
                                , functionAttributes :: [String]
                                , functionBody :: [Node]
                                }
          | LetStatement { letLexpr :: String
                         , letValue :: String
                         }
          | UnletStatement { unletBang :: Bool
                           , unletBody :: String
                           }

          -- Docvim nodes: "block-level" elements
          | DocBlock [Node]
          | Paragraph [Node]
          | LinkTargets [String]
          | List [Node]
          | ListItem [Node]
          | Blockquote [Node]
          | Fenced [String]

          -- Docvim nodes: "phrasing content" elements
          | Plaintext String
          | BreakTag
          | Link String
          | Code String
          | Whitespace

          -- Docvim nodes: annotations
          | PluginAnnotation Name Description
          | FunctionAnnotation Name -- not sure if I will want more here
          | IndentAnnotation
          | DedentAnnotation
          | CommandAnnotation Usage
          | FooterAnnotation
          | MappingsAnnotation
          | MappingAnnotation Name
          | OptionAnnotation Name Type (Maybe Default)
          | HeadingAnnotation String
          | SubheadingAnnotation String
  deriving (Eq, Show)

-- The VimScript (VimL) grammar is embodied in the implementation of
-- https://github.com/vim/vim/blob/master/src/eval.c; there is no formal
-- specification for it, and there are many ambiguities that can only be
-- resolved at runtime. We aim to parse a loose subset.

-- TODO: deal with bar |
--       note that `function X() |` does not work, and `endf` must be on a line
--       of its own too (not a syntax error to do `| endf`, but it doesn't work
--       , so you need to add another `endf`, which will blow up at runtime.
-- TODO: validate name = CapitalLetter or s:foo or auto#loaded

data ArgumentList = ArgumentList [Argument]
  deriving (Eq, Show)

data Argument = Argument String
  deriving (Eq, Show)

type Default = String
type Description = String
type Name = String
type Type = String
type Usage = String

-- | Walks an AST node calling the supplied visitor function.
--
-- This is an in-order traversal.
--
-- For example, to implement a visitor which counts all nodes:
--
-- >  import Data.Monoid
-- >  counter i _ = mappend i 1
-- >  count = getSum $ walk counter (Sum 0) tree
--
-- Another example; accumulating `SubheadingAnnotation` nodes into a list:
--
-- >  accumulator nodes node@(SubheadingAnnotation _) = mappend nodes [node]
-- >  accumulator nodes _ = nodes -- skip everything else
-- >  nodes = walk accumulator [] tree
--
walk :: Monoid a => (a -> Node -> a) -> a -> Node -> a
walk f acc n = foldl (walk f) (f acc n) children
  where
    children = case n of
      DocBlock d             -> d
      FunctionDeclaration {} -> functionBody n
      List l                 -> l
      ListItem i             -> i
      Blockquote b           -> b
      Paragraph p            -> p
      Unit u                 -> u
      _                      -> [] -- no Node children
