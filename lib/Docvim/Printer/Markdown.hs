module Docvim.Printer.Markdown
  ( markdown
  , pm
  , ppm
  ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Docvim.AST
import Docvim.Parse (parseUnit)

markdown :: Unit -> String
markdown (Unit nodes) = concatMap md nodes

md :: Node -> String
md (DocBlock d) = concatMap node d
md (Link l) = "|" ++ l ++ "|" -- TODO: actual links

node :: Node -> String
node (Blockquote b) = blockquote b ++ "\n\n"
node (BreakTag) = "<br />"
node (Code c) = "`" ++ c ++ "`"
node (Fenced f) = fenced f ++ "\n\n"
node (HeadingAnnotation h) = "# " ++ h ++ "\n\n"
node (LinkTargets l) = linkTargets l ++ "\n"
node (List ls) = (concatMap node ls) ++ "\n"
node (ListItem l) = "- " ++ (concatMap node l) ++ "\n"
node (Paragraph p) = (concatMap node p) ++ "\n\n"
node (Plaintext p) = p
node (SubheadingAnnotation s) = "## " ++ s ++ "\n\n"
node (Whitespace) = " "

blockquote :: [Node] -> String
blockquote ps = "> " ++ intercalate "\n>\n> " (map paragraph ps)
  where
    -- Strip off trailing newlines from each paragraph
    paragraph p = take ((length (node p)) - 2) (node p)

fenced :: [String] -> String
fenced f = "```\n" ++ code ++ "```"
  where code = if length f == 0
               then ""
               else (intercalate "\n" f) ++ "\n"

-- TODO sanitize fragments
linkTargets :: [String] -> String
linkTargets ls =  "<p align=\"right\">"
               ++ intercalate " " (map linkify ls)
               ++ "</p>"
  -- TODO make fn for templating HTML...
  where linkify l =  "<a name=\"" ++ sanitizeAnchorName l ++ "\" "
                  ++ "href=\"#" ++ gitHubAnchorName l ++ "\">"
                  ++ l
                  ++ "</a>"

-- | Sanitizes a link target similar to the way that GitHub does:
--
--    - Downcase.
--    - Filter, keeping only letter, number, space, hyphen.
--    - Change spaces to hyphens.
--    - Uniquify by appending "-1", "-2", "-3" etc.
--
-- Source: https://gist.github.com/asabaylus/3071099#gistcomment-1593627
sanitizeAnchorName :: String -> String
sanitizeAnchorName = hyphenate . keepValid . downcase
  where
    hyphenate = map spaceToHyphen
    spaceToHyphen c = if c == ' ' then '-' else c
    keepValid = filter (`elem` (['a'..'z'] ++ ['0'..'9'] ++ " -"))
    downcase = map toLower

gitHubAnchorName :: String -> String
gitHubAnchorName n = "user-content-" ++ sanitizeAnchorName n

-- | For unit testing.
pm :: String -> String
pm input = case parseUnit input of
            Left error -> show error
            Right ast -> markdown ast

-- | For logging in the REPL.
ppm :: String -> IO ()
ppm = putStr . pm
