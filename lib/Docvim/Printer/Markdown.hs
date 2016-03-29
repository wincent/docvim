module Docvim.Printer.Markdown
  ( markdown
  , pm
  , ppm
  ) where

import Docvim.AST
import Docvim.Parse (parseUnit)

markdown :: Unit -> String
markdown (Unit nodes) = concatMap md nodes

md :: Node -> String
md (DocBlock d) = concatMap node d
md (Link l)     = "|" ++ l ++ "|" -- TODO: actual links
md (Whitespace) = " "
-- TODO: etc...

node :: Node -> String
node (BreakTag) = "<br />"
node (Code c)     = "`" ++ c ++ "`"
node (HeadingAnnotation h) = "# " ++ h ++ "\n\n"
node (Paragraph p) = (concatMap node p) ++ "\n\n"
node (Plaintext p) = p
node (SubheadingAnnotation s) = "## " ++ s ++ "\n\n"
node (Whitespace) = " "

-- | For unit testing.
pm :: String -> String
pm input = case parseUnit input of
            Left error -> show error
            Right ast -> markdown ast

-- | For logging in the REPL.
ppm :: String -> IO ()
ppm = putStr . pm
