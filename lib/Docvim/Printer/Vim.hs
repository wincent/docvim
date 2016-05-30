module Docvim.Printer.Vim
  ( vimHelp
  , ppv
  , pv
  ) where

import Control.Monad.Reader
import Data.Char (toUpper)
import Data.List (intercalate)
import Docvim.AST
import Docvim.Parse (parseUnit, strip)
import Docvim.Visitor.Plugin (getPluginName)
import Docvim.Visitor.Symbol (getSymbols)

-- TODO: taken straight out of Markdown.hs; DRY this up
data State = State { symbols :: [String]
                   , pluginName :: Maybe String
                   }
type Env = Reader State String

vimHelp :: Node -> String
vimHelp n = strip $ runReader (node n) state
  where state = State (getSymbols n) (getPluginName n)

nodes :: [Node] -> Env
nodes ns = concat <$> mapM node ns

-- TODO: deal with hard-wrapping
node :: Node -> Env
node n = case n of
  -- Nodes that depend on (or must propagate) reader context.
  Blockquote b            -> blockquote b >>= nl >>= nl
  DocBlock d              -> nodes d
  FunctionDeclaration {}  -> nodes $ functionBody n
  Paragraph p             -> nodes p >>= nl >>= nl
  Link l                  -> link l
  List ls                 -> nodes ls >>= nl
  ListItem l              -> fmap ("- " ++) (nodes l) >>= nl
  Project p               -> nodes p
  Unit u                  -> nodes u

  -- Nodes that don't depend on reader context.
  BreakTag                -> return "\n"
  Code c                  -> return $ "`" ++ c ++ "`"
  Fenced f                -> return $ fenced f ++ "\n\n"
  -- TODO: Vim will only highlight this as a heading if it has a trailing
  -- LinkTarget on the same line; figure out how to handle that; may need to
  -- address it in the Parser
  --
  -- Looking at the Ferret fixtures, seems like I had an idea for this which was
  -- to auto-gen the targets based on the plugin name + the heading text.
  --
  -- I could also just make people specify a target explicitly.
  HeadingAnnotation h     -> return $ map toUpper h ++ "\n\n"
  LinkTargets l           -> return $ linkTargets l ++ "\n"
  -- TODO: this should be order-independent and always appear at the top.
  -- Note that I don't really have anywhere to put the description; maybe I should
  -- scrap it (nope: need it in the Vim help version).
  PluginAnnotation name desc -> return $ plugin name desc
  Plaintext p             -> return p
  Separator               -> return $ "---" ++ "\n\n"
  SubheadingAnnotation s  -> return $ s ++ " ~\n\n"
  -- TODO deal with hard wrapping: here might be a good place for it...
  Whitespace              -> return " "
  _                       -> return ""

-- TODO: right-align trailing link target
-- TODO: add {name}.txt to the symbol table?
plugin name desc =
  "*" ++ name ++ ".txt*" ++
  "    " ++ desc ++ "      " ++
  "*" ++ name ++ "*"

-- | Append a newline.
nl :: String -> Env
nl = return . (++ "\n")

-- TODO fix 1-line blockquote case
blockquote :: [Node] -> Env
blockquote ps = do
  ps' <- mapM paragraph ps
  return $ "    " ++ intercalate "\n\n    " ps'
  where
    -- Strip off trailing newlines from each paragraph.
    paragraph p = fmap trim (node p)
    trim contents = take (length contents - 2) contents

-- TODO: handle "interesting" link text like containing [, ], "
link :: String -> Env
link l = do
  state <- ask
  return $ if l `elem` symbols state
           -- TODO: beware names with < ` etc in them
           then "|" ++ l ++ "|"
           -- TODO: figure out what to do here
           -- probably want to treat URLs specially
           -- and Vim help links, obviously
           else l

-- TODO ideally want to replace preceding blank line with >, not append one
-- and likewise, replace following blank line with <
-- (but this will be tricky)
fenced :: [String] -> String
fenced f = ">\n" ++ code ++ "<\n"
  where code = if null f
               then ""
               else "    " ++ (intercalate "\n    " f ++ "\n")

linkTargets :: [String] -> String
linkTargets ls = unwords (map linkify ls)
  -- TODO: make this actually useful
  where linkify l = "*" ++ l ++ "*"

-- | For unit testing.
pv :: String -> String
pv input = case parseUnit input of
            Left error -> show error
            Right ast -> vimHelp ast

-- | For logging in the REPL.
ppv :: String -> IO ()
ppv = putStr . pv
