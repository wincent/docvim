module Docvim.Printer.Markdown
  ( markdown
  , pm
  , ppm
  ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Docvim.AST
import Docvim.Parse (parseUnit)

data Anchor = Anchor [Attribute] String
data Attribute = Attribute { attributeName :: String
                           , attributeValue :: String
                           }

markdown :: Node -> String
markdown (Unit nodes) = concatMap node nodes

node :: Node -> String
node n = case n of
  (Blockquote b)            -> blockquote b ++ "\n\n"
  BreakTag                  -> "<br />"
  (Code c)                  -> "`" ++ c ++ "`"
  (DocBlock d)              -> concatMap node d
  (Fenced f)                -> fenced f ++ "\n\n"
  (HeadingAnnotation h)     -> "## " ++ h ++ "\n\n"
  (Link l)                  -> link l
  (LinkTargets l)           -> linkTargets l ++ "\n"
  (List ls)                 -> concatMap node ls ++ "\n"
  (ListItem l)              -> "- " ++ concatMap node l ++ "\n"
   -- TODO: this should be order-independent and always appear at the top.
   -- Note that I don't really have anywhere to put the description; maybe I should
   -- scrap it.
  (PluginAnnotation name _) -> "# " ++ name ++ "\n\n"
  (Paragraph p)             -> concatMap node p ++ "\n\n"
  (Plaintext p)             -> p
  (SubheadingAnnotation s)  -> "### " ++ s ++ "\n\n"
  Whitespace                -> " "
  _                         -> ""

blockquote :: [Node] -> String
blockquote ps = "> " ++ intercalate "\n>\n> " (map paragraph ps)
  where
    -- Strip off trailing newlines from each paragraph
    paragraph p = let contents = node p
                  in take (length contents - 2) contents

fenced :: [String] -> String
fenced f = "```\n" ++ code ++ "```"
  where code = if null f
               then ""
               else intercalate "\n" f ++ "\n"

-- TODO: handle "interesting" link text like containing [, ], "
-- TODO: handle other kinds of links (ie. using symbol table, distinguish links
-- to internal vim functionality, like |:highlight| etc
link :: String -> String
link l = "[" ++ l ++ "](" ++ gitHubAnchor l ++ ")"

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
