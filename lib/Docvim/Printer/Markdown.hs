module Docvim.Printer.Markdown
  ( markdown
  , pm
  , ppm
  ) where

import Data.List (intercalate)
import Docvim.AST
import Docvim.Parse (parseUnit)

markdown :: Unit -> String
markdown (Unit nodes) = concatMap md nodes

md :: Node -> String
md (DocBlock d) = concatMap node d
md (Link l) = "|" ++ l ++ "|" -- TODO: actual links

-- TODO: group list items together
node :: Node -> String
node (Blockquote b) = blockquote b ++ "\n\n"
node (BreakTag) = "<br />"
node (Code c) = "`" ++ c ++ "`"
node (HeadingAnnotation h) = "# " ++ h ++ "\n\n"
node (ListItem l) = "- " ++ (concatMap node l) ++ "\n\n"
node (Paragraph p) = (concatMap node p) ++ "\n\n"
node (Plaintext p) = p
node (SubheadingAnnotation s) = "## " ++ s ++ "\n\n"
node (Whitespace) = " "

blockquote :: [Node] -> String
blockquote ps = "> " ++ intercalate "\n>\n> " (map paragraph ps)
  where
    -- Strip off trailing newlines from each paragraph
    paragraph p = take ((length (node p)) - 2) (node p)

-- | For unit testing.
pm :: String -> String
pm input = case parseUnit input of
            Left error -> show error
            Right ast -> markdown ast

-- | For logging in the REPL.
ppm :: String -> IO ()
ppm = putStr . pm
