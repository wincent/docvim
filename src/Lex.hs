module Lex (l, lexTokens) where

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
import Text.Parsec
  ( (<|>)
  , (<?>)
  , many
  , many1
  , runParser
  )
import Text.Parsec.Prim (parseTest)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Char (anyChar, char, noneOf, string)

data Token = Annotation String
           | Blockquote String
           | Code String
           | CommentStart String
           | DocBlockStart String
           | Heading String
           | Link String
           | LinkTarget String
           | PreFence String
           | Separator String
           | SubHeading String
           | VimScriptLine String -- later, this gets more complicated
           | Whitespace String
  deriving (Eq, Show)

-- | Parses a translation unit (file contents) into an AST.
lexUnit :: Parser [Token]
lexUnit = many $ VimScriptLine <$> string "thing"

-- Called `lexTokens` instead of `lex` to avoid conflict with `Prelude.lex`.
lexTokens :: String -> IO [Token]
lexTokens fileName = parseFromFile lexUnit fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

-- | To facilitate quick testing in the console.
-- import Lex (l)
-- l "test"
l = parseTest lexUnit
