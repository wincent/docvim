module Docvim.Lex (l, lexTokens) where

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
import Text.Parsec ( (<|>)
                   , (<?>)
                   , choice
                   , lookAhead
                   , many
                   , many1
                   , manyTill
                   , runParser
                   , try
                   )
import Text.Parsec.Prim (parseTest)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Char (anyChar, char, noneOf, oneOf, string)

data Token = Annotation String
           | Blockquote
           | Code String
           | CommentStart
           | DocBlockStart
           | Heading
           | Link String
           | LinkTarget String
           | ListItem
           | Newline
           | PreFence
           | Separator
           | SubHeading
           | VimScriptLine String -- later, this gets more complicated
           | Whitespace String
           | Word String
  deriving (Eq, Show)

-- | Parses a translation unit (file contents) into an AST.
lexUnit :: Parser [Token]
lexUnit = many token <* eof

-- TODO: literal <br /> support
token = choice [ --vimScriptLine -- must come first
               {-,-} annotation
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
               , separator -- must come before listItem
               , listItem
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
commentStart  = string "\"" >> return CommentStart
blockquote    = string ">" >> return Blockquote
code          = Code <$> (char '`' *> many1 (noneOf "`") <* char '`')
docBlockStart = try $ string "\"\"" >> return DocBlockStart
heading       = string "#" >> return Heading
link          = Link <$> (char '|' *> many1 (noneOf "|") <* char '|')
linkTarget    = LinkTarget <$> (char '*' *> many1 (noneOf "*") <* char '*')
listItem      = string "-" >> return ListItem
newline       = string "\n" >> return Newline
preFence      = try $ string "```" >> return PreFence
separator     = try $ string "---" >> return Separator
subHeading    = try $ string "##" >> return SubHeading
-- type error
-- vimScriptLine = VimScriptLine <$> (manyTill anyChar (newline <|> (lookAhead eof)))

-- "works", but it is too greedy
-- vimScriptLine = try $ VimScriptLine <$> manyTill anyChar newline

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
-- import Docvim.Lex (l)
-- l "test"
l = parseTest lexUnit
