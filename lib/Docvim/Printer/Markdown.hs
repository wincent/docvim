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
markdown (Unit nodes) = concatMap md nodes

md :: Node -> String
md (DocBlock d) = concatMap node d
md (Link l) = "|" ++ l ++ "|" -- TODO: actual links

node :: Node -> String
node (Blockquote b) = blockquote b ++ "\n\n"
node BreakTag = "<br />"
node (Code c) = "`" ++ c ++ "`"
node (Fenced f) = fenced f ++ "\n\n"
node (HeadingAnnotation h) = "## " ++ h ++ "\n\n"
node (Link l) = link l
node (LinkTargets l) = linkTargets l ++ "\n"
node (List ls) = concatMap node ls ++ "\n"
node (ListItem l) = "- " ++ concatMap node l ++ "\n"
-- TODO: this should be order-independent and always appear at the top.
-- Note that I don't really have anywhere to put the description; maybe I should
-- scrap it.
node (PluginAnnotation name _) = "# " ++ name ++ "\n\n"
node (Paragraph p) = concatMap node p ++ "\n\n"
node (Plaintext p) = p
node (SubheadingAnnotation s) = "### " ++ s ++ "\n\n"
node Whitespace = " "

blockquote :: [Node] -> String
blockquote ps = "> " ++ intercalate "\n>\n> " (map paragraph ps)
  where
    -- Strip off trailing newlines from each paragraph
    paragraph p = take (length (node p) - 2) (node p)

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
