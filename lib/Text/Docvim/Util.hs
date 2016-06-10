-- | Functions to facilitate automated and manual testing.
module Text.Docvim.Util ( compileUnit
                        , p
                        , pm
                        , pp
                        , ppm
                        , ppv
                        , pv
                        ) where

import Text.Docvim.AST
import Text.Docvim.Compile
import Text.Docvim.Parse
import Text.Docvim.Printer.Markdown
import Text.Docvim.Printer.Vim
import Text.Parsec
import Text.Show.Pretty

-- | Parse and compile a string containing a translation unit.
compileUnit :: String -> Either ParseError Node
compileUnit input = do
  parsed <- runParser unit () "(eval)" input
  return $ compile [parsed]

-- | Convenience function: Parse and compile a string containing a translation
-- unit, but always returns a string even in the case of an error.
p :: String -> String
p input = case compileUnit input of
            Left error -> show error
            Right ast -> ppShow ast

-- | Pretty-prints the result of parsing and compiling an input string.
--
-- To facilitate quick testing in the REPL; example:
--
--     pp "unlet g:var"
pp :: String -> IO ()
pp = putStrLn . p

-- | Parse and compile an input string into Vim help format.
pv :: String -> String
pv input = case compileUnit input of
            Left error -> show error
            Right ast -> vimHelp ast

-- | Pretty-prints the result of parsing and compiling an input string and
-- transforming into Vim help format.
--
-- For logging in the REPL.
ppv :: String -> IO ()
ppv = putStr . pv

-- | Parse and compile an input string into Markdown help format.
pm :: String -> String
pm input = case compileUnit input of
            Left error -> show error
            Right ast -> markdown ast

-- | Pretty-prints the result of parsing and compiling an input string and
-- transforming into Markdown format.
--
-- For logging in the REPL.
ppm :: String -> IO ()
ppm = putStr . pm
