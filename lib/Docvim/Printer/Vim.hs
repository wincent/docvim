module Docvim.Printer.Vim
  ( vimHelp
  , ppv
  , pv
  ) where

import Data.Char (toUpper)
import Docvim.AST
import Docvim.Parse (parseUnit)

vimHelp :: Node -> String
vimHelp (Project nodes) = concatMap vh nodes
vimHelp _ = "" -- TODO: figure out what to do here

vh :: Node -> String
vh (DocBlock d) = concatMap node d

node :: Node -> String
-- TODO: Vim will only highlight this as a heading if it has a trailing
-- LinkTarget on the same line; figure out how to handle that; may need to
-- address it in the Parser
node (HeadingAnnotation h) = map toUpper h ++ "\n\n"

node (PluginAnnotation name desc) =
  "*" ++ name ++ ".txt*" ++
  "    " ++ desc ++ "      " ++
  "*" ++ name ++ "*"

node (SubheadingAnnotation s) = s ++ " ~\n\n"
node _ = "[not yet implemented]"

-- | For unit testing.
pv :: String -> String
pv input = case parseUnit input of
            Left error -> show error
            Right ast -> vimHelp ast

-- | For logging in the REPL.
ppv :: String -> IO ()
ppv = putStr . pv
