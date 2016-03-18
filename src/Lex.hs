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
-- TODO: custom error messages with <?>
import Text.Parsec
  ( (<|>)
  , (<?>)
  , many
  , many1
  , runParser
  )
import Text.Parsec (choice, try)
import Text.Parsec.Prim (parseTest)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Char (anyChar, char, noneOf, oneOf, string)

data Token = Annotation String
           | Blockquote String
           | Code String
           | CommentStart String
           | DocBlockStart String
           | Heading String
           | Link String
           | LinkTarget String
           | Newline String
           | PreFence String
           | Separator String
           | SubHeading String
           | VimScriptLine String -- later, this gets more complicated
           | Whitespace String
           | Word String
  deriving (Eq, Show)

-- | Parses a translation unit (file contents) into an AST.
lexUnit :: Parser [Token]
lexUnit = (many token) <* eof

token = choice [ annotation
               , blockquote
               , preFence -- must come before code
               , code
               , docBlockStart -- must come before commentStart
               , commentStart
               , subHeading -- must come before heading
               , heading
               , link
               , linkTarget
               , newline
               , separator
               -- , vimScriptLine
               , whitespace
               , word
               ]
               <?> "valid token"

annotationName = choice [ string "command"
                        , string "dedent"
                        , string "footer"
                        , string "indent"
                        , string "mappings" -- must come before mapping
                        , string "mapping"
                        , string "option"
                        , string "plugin"
                        ]

annotation    = Annotation <$> (char '@' *> annotationName)
commentStart  = CommentStart <$> string "\""
blockquote    = Blockquote <$> string ">"
code          = Code <$> (char '`' *> many1 (noneOf "`") <* char '`')
docBlockStart = try $ DocBlockStart <$> string "\"\""
heading       = Heading <$> string "#"
link          = Link <$> (char '|' *> many1 (noneOf "|") <* char '|')
linkTarget    = LinkTarget <$> (char '*' *> many1 (noneOf "*") <* char '*')
newline       = Newline <$> string "\n"
preFence      = try $ PreFence <$> string "```"
separator     = Separator <$> string "---"
subHeading    = try $ Heading <$> string "##"
whitespace    = Whitespace <$> many1 (oneOf " \t")
word          = Word <$> many1 (noneOf " \t\n")

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
