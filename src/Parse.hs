module Parse (p, parse) where

import Control.Applicative
  ( (*>)
  , (<$)
  , (<$>)
  , (<*)
  , (<*>)
  )
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
-- TODO: custom error messages with <?>
import Text.Parsec
  ( (<|>)
  , (<?>)
  , many
  , many1
  , runParser
  )
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Char (anyChar, char, noneOf, string)

data Node = TranslationUnit [Node]
          | VimScript String
          | Comment String
          -- ...everything else
  deriving (Eq, Show)

-- | Parses a translation unit (file contents) into an AST.
parseUnit :: Parser Node
parseUnit =   TranslationUnit
          <$> many parseNode
          <*  eof

parseNode :: Parser Node
parseNode =   parseVimScript
          <|> parseComment

-- TODO: for now skip vimscript; later, parse it and use it to enrich the docs
-- or take some manual work out of specifying what's going on
parseVimScript :: Parser Node
parseVimScript =   VimScript
               <$> many1 (noneOf ['"']) -- anyChar

parseComment :: Parser Node
parseComment =   Comment
             <$> (char '"' *> many1 (noneOf ['\n']))

parse :: String -> IO Node
parse fileName = parseFromFile parseUnit fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

-- | To facilitate quick testing in the console.
-- import Parse (p)
-- p "test"
p = runParser parseUnit () "(eval)"
