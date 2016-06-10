-- | Functions to facilitate automated and manual testing.
module Text.Docvim.Util ( compileUnits
                        , p
                        , parseUnit
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

-- | Parse a string containing a translation unit.
parseUnit :: String -> Either ParseError Node
parseUnit = runParser unit () "(eval)"

-- | Parse and compile a list of strings containing a translation units.
compileUnits :: [String] -> Either ParseError Node
compileUnits inputs = do
  parsed <- mapM parseUnit inputs
  return $ compile parsed

-- | Convenience function: Parse and compile a list of strings containing
-- translation units, but always returns a string even in the case of an error.
p :: [String] -> String
p inputs = case compileUnits inputs of
    Left err -> show err
    Right ast -> ppShow ast

-- | Pretty-prints the result of parsing and compiling an input string.
--
-- To facilitate quick testing in the REPL; example:
--
--     pp "unlet g:var"
pp :: String -> IO ()
pp input = putStrLn $ p [input]

-- | Parse and compile a list of input strings into Vim help format.
pv :: [String] -> String
pv inputs = case compileUnits inputs of
    Left err -> show err
    Right ast -> vimHelp ast

-- | Pretty-prints the result of parsing and compiling an input string and
-- transforming into Vim help format.
--
-- For logging in the REPL.
ppv :: String -> IO ()
ppv input = putStr $ pv [input]

-- | Parse and compile a list of input strings into Markdown help format.
pm :: [String] -> String
pm inputs = case compileUnits inputs of
    Left err -> show err
    Right ast -> markdown ast

-- | Pretty-prints the result of parsing and compiling an input string and
-- transforming into Markdown format.
--
-- For logging in the REPL.
ppm :: String -> IO ()
ppm input = putStr $ pm [input]
