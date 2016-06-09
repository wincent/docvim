{-# LANGUAGE DeriveDataTypeable #-}

module Docvim.AST where

import Control.Lens.Fold (foldlOf)
import Control.Lens.Getter (to)
import Control.Lens.Plated (Plated, cosmosOf)
import Data.Char (toLower)
import Data.Data
import Data.Data.Lens (uniplate)
import Data.List (intercalate)
import Data.Monoid ((<>))

data Node
          -- Roots
          = Project [Node] -- list of translation units (files)
          | Unit [Node] -- translation unit (file)

          -- To remove any node from the tree, we can just replace it with this.
          | Empty

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
          | LexprStatement { lexprBang :: Bool
                           , lexprExpr :: String
                           }
          | LwindowStatement { lwindowHeight :: Maybe Int }
          | UnletStatement { unletBang :: Bool
                           , unletBody :: String
                           }
          | GenericStatement String -- ^ For the stuff we only crudely parse for now.

          -- Docvim nodes: "block-level" elements
          | DocBlock [Node]
          | Paragraph [Node]
          | LinkTargets [String]
          | List [Node]
          | ListItem [Node]
          | Blockquote [Node]
          | Fenced [String]
          | Separator

          -- Docvim nodes: "phrasing content" elements
          | Plaintext String
          | BreakTag
          | Link String
          | Code String
          | Whitespace

          -- Docvim nodes: annotations
          | PluginAnnotation Name Description
          | FunctionsAnnotation
          | FunctionAnnotation Name -- not sure if I will want more here
          | IndentAnnotation
          | DedentAnnotation
          | CommandsAnnotation
          | CommandAnnotation Name (Maybe Parameters)
          | FooterAnnotation
          | MappingsAnnotation
          | MappingAnnotation Name
          | OptionsAnnotation
          | OptionAnnotation Name Type (Maybe Default)
          | HeadingAnnotation String
          | SubheadingAnnotation String

          -- Docvim nodes: synthesized nodes
          | TOC [String]
  deriving (Data, Eq, Show, Typeable)

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
  deriving (Data, Eq, Show, Typeable)

data Argument = Argument String
  deriving (Data, Eq, Show, Typeable)

instance Plated Node

type Default = String
type Description = String
type Name = String
type Type = String
type Parameters = String

-- | Walks an AST node calling the supplied visitor function.
--
-- This is an in-order traversal.
--
-- For example, to implement a visitor which counts all nodes:
--
-- >  import Data.Monoid
-- >  count = getSum $ walk (\_ -> 1) (Sum 0) tree
--
-- For comparison, here is a (probably inefficient) alternative way using
-- `Control.Lens.Plated` instead of `walk`:
--
-- > import Control.Lens.Operators
-- > import Control.Lens.Plated
-- > import Data.Data.Lens
-- > count = length $ tree ^.. cosmosOf uniplate
--
-- Another example; accumulating `SubheadingAnnotation` nodes into a list:
--
-- >  accumulator node@(SubheadingAnnotation _) = [node]
-- >  accumulator _ = [] -- skip everything else
-- >  nodes = walk accumulator [] tree
--
-- Again, for comparison, the same without `walk`, this time using a list
-- comprehension:
--
-- > import Control.Lens.Operators
-- > import Control.Lens.Plated
-- > import Data.Data.Lens
-- > [n | n@(SubheadingAnnotation _) <- tree ^.. cosmosOf uniplate]
--
walk :: Monoid a => (Node -> a) -> a -> Node -> a
walk f = foldlOf (cosmosOf uniplate . to f) (<>)
-- TODO: consider making it possible for `f` to return `Nothing` to
-- short-circuit traversal, or `Just a` to continue with the current `mappend`
-- behavior.

-- | Sanitizes a link target similar to the way that GitHub does:
--
--    - Downcase.
--    - Filter, keeping only letter, number, space, hyphen.
--    - Change spaces to hyphens.
--    - Uniquify by appending "-1", "-2", "-3" etc (not yet implemented).
--
-- We use this both for generating GitHub friendly link targets, and for
-- auto-generating new link targets for use inside Vim help files.
--
-- Source: https://gist.github.com/asabaylus/3071099#gistcomment-1593627
sanitizeAnchor :: String -> String
sanitizeAnchor = hyphenate . keepValid . downcase
  where
    hyphenate = map spaceToHyphen
    spaceToHyphen c = if c == ' ' then '-' else c
    keepValid = filter (`elem` (['a'..'z'] ++ ['0'..'9'] ++ " -"))
    downcase = map toLower
