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
md (Code c)     = "`" ++ c ++ "`"
md (DocBlock d) = concatMap node d
md (Link l)     = "|" ++ l ++ "|" -- TODO: actual links
md (Whitespace) = " "
-- TODO: etc...

node :: Node -> String
node (HeadingAnnotation h) = "# " ++ h ++ "\n"

-- | For unit testing.
pm :: String -> String
pm input = case parseUnit input of
            Left error -> show error
            Right ast -> markdown ast

-- | For logging in the REPL.
ppm :: String -> IO ()
ppm = putStrLn . pm
