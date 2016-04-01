module Docvim.Printer.Markdown
  ( markdown
  , pm
  , ppm
  ) where

import Control.Monad.Reader
import Data.Char (toLower)
import Data.List (intercalate)
import Docvim.AST
import Docvim.Parse (parseUnit)
import Docvim.Visitor.Symbol (getSymbols)

type Symbols = [String]
type Printer = Reader Symbols String

data Anchor = Anchor [Attribute] String
data Attribute = Attribute { attributeName :: String
                           , attributeValue :: String
                           }

-- TODO: consider relaxing the pattern here to accept more than Unit
-- (might want to accept any block-level element, or perhaps even any node at
-- all)
markdown :: Node -> String
markdown unit@(Unit ns) = concatMap (\n -> runReader (node n) symbols) ns
  where
    symbols = getSymbols unit

nodes :: [Node] -> Printer
nodes ns = do
  symbols <- ask
  return $ concatMap (\n -> runReader (node n) symbols) ns

node :: Node -> Printer
node n = case n of
  -- Nodes that depend on reader context.
  (Blockquote b) -> do
    b' <- blockquote b
    return $ b' ++ "\n\n"

  (DocBlock d) -> nodes d

  (Paragraph p) -> do
    p' <- nodes p
    return $ p' ++ "\n\n"

  (Link l) -> link l

  (List ls) -> do
    ls' <- nodes ls
    return $ ls' ++ "\n"

  (ListItem l) -> do
    l' <- nodes l
    return $ "- "  ++ l' ++ "\n"

  -- Nodes that don't depend on reader context.
  BreakTag                  -> return "<br />"
  (Code c)                  -> return $ "`" ++ c ++ "`"
  (Fenced f)                -> return $ fenced f ++ "\n\n"
  (HeadingAnnotation h)     -> return $ "## " ++ h ++ "\n\n"
  (LinkTargets l)           -> return $ linkTargets l ++ "\n"
  -- TODO: this should be order-independent and always appear at the top.
  -- Note that I don't really have anywhere to put the description; maybe I should
  -- scrap it.
  (PluginAnnotation name _) -> return $ "# " ++ name ++ "\n\n"
  (Plaintext p)             -> return p
  Separator                 -> return $ "---" ++ "\n\n"
  (SubheadingAnnotation s)  -> return $ "### " ++ s ++ "\n\n"
  Whitespace                -> return " "
  _                         -> return ""

blockquote :: [Node] -> Printer
blockquote ps = do
  symbols <- ask
  return $ "> " ++ intercalate "\n>\n> " (paragraphs symbols)
  where
    -- Strip off trailing newlines from each paragraph.
    paragraph p = do
      contents <- node p
      return $ take (length contents - 2) contents
    paragraphs symbols = map (\n -> runReader (paragraph n) symbols) ps

-- TODO: handle "interesting" link text like containing [, ], "
link :: String -> Printer
link l = do
  symbols <- ask
  return $ if l `elem` symbols
           then "[" ++ l ++ "](" ++ gitHubAnchor l ++ ")"
           else "`" ++ l ++ "`" -- TODO: decide on styling here,
                                -- may want to try producing a link to Vim
                                -- online help if I can find a search for it

fenced :: [String] -> String
fenced f = "```\n" ++ code ++ "```"
  where code = if null f
               then ""
               else intercalate "\n" f ++ "\n"

linkTargets :: [String] -> String
linkTargets ls =  "<p align=\"right\">"
               ++ unwords (map linkify ls)
               ++ "</p>"
  where linkify l = a $ Anchor [ Attribute "name" (sanitizeAnchor l)
                               , Attribute "href" (gitHubAnchor l)
                               ]
                               l

a :: Anchor -> String
a (Anchor attributes target) = "<a" ++ attrs ++ ">" ++ target ++ "</a>"
  where
    attrs = if not (null attributes)
            then " " ++ attributesString attributes
            else ""

attributesString :: [Attribute] -> String
attributesString as = unwords (map attributeToString as)
  where attributeToString (Attribute name value) = name ++ "=\"" ++ value ++ "\""

-- | Sanitizes a link target similar to the way that GitHub does:
--
--    - Downcase.
--    - Filter, keeping only letter, number, space, hyphen.
--    - Change spaces to hyphens.
--    - Uniquify by appending "-1", "-2", "-3" etc (not yet implemented).
--
-- Source: https://gist.github.com/asabaylus/3071099#gistcomment-1593627
sanitizeAnchor :: String -> String
sanitizeAnchor = hyphenate . keepValid . downcase
  where
    hyphenate = map spaceToHyphen
    spaceToHyphen c = if c == ' ' then '-' else c
    keepValid = filter (`elem` (['a'..'z'] ++ ['0'..'9'] ++ " -"))
    downcase = map toLower

gitHubAnchor :: String -> String
gitHubAnchor n = "#user-content-" ++ sanitizeAnchor n

-- | For unit testing.
pm :: String -> String
pm input = case parseUnit input of
            Left error -> show error
            Right ast -> markdown ast

-- | For logging in the REPL.
ppm :: String -> IO ()
ppm = putStr . pm
