module Parse (p, parse) where

import Control.Applicative ( (*>)
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
                   , optionMaybe
                   , optional
                   , runParser
                   , skipMany
                   , try
                   )
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Char ( alphaNum
                                          , anyChar
                                          , char
                                          , noneOf
                                          , oneOf
                                          , string
                                          )

data Node = DocComment [DocNode]
          | VimScript String
  deriving (Eq, Show)

-- | Textual tokens recognized during parsing but not embedded in the AST.
data Token = Blockquote
           | CommentStart
           | DocBlockStart
           | ListItem
           | Newline
           | Whitespace String
           | EOF
  deriving (Eq, Show)

data DocNode = DocNode Annotation
             | Heading String
  deriving (Eq, Show)

type Default = String
type Description = String
type Name = String
type Type = String
type Usage = String
data Annotation = Plugin Name Description
                | Indent
                | Dedent
                | Command Usage
                | Footer
                | Mappings
                | Mapping Name
                | Option Name Type (Maybe Default)
  deriving (Eq, Show)

vimScriptLine = VimScript <$> many1 (noneOf "\n") <* (optional newline)

-- These cause type errors unless used...
-- blockquote    = string ">" >> return Blockquote
-- commentStart  = string "\"" >> return CommentStart
docBlockStart = (string "\"\"" <* (optional ws)) >> return DocBlockStart
-- listItem      = string "-" >> return ListItem
newline       = char '\n' >> return Newline
ws    = Whitespace <$> many1 (oneOf " \t")

docComment :: Parser Node
docComment = do
  optional ws
  -- docBlockStart
  -- comment <- DocComment <$> many1 docNode
  -- skipMany vimScriptLine
  -- return comment

  -- want to conditionally choose between docNode and vimScriptLine based on
  -- whether we succeeded in peeking at and consuming docBlockStart
  maybeDocBlock <- optionMaybe $ try docBlockStart
  case maybeDocBlock of
    Just a -> DocComment <$> many1 docNode
    -- TODO: parse the VimScript too and extract metadata from it to attach to
    -- DocComment node
    Nothing -> vimScriptLine -- would like to use skipMany here

docNode :: Parser DocNode
docNode = choice [ annotation
                 , heading
                 ]

heading :: Parser DocNode
heading = Heading <$> (char '#' >> (optional ws) *> (manyTill anyChar (newline <|> (eof >> return EOF))))
-- TODO: probably want to swallow the newline here; make it implicit
-- (and any trailing whitespace)

-- | Match a "word" of non-whitespace characters.
word = many1 (noneOf " \n\t")

-- | Run a parser and consume trailing whitespace.
lexeme parser = do
  result <- parser
  ws
  return result -- could also just do (parser <* ws)
-- ^ not sure if I want to use this yet, as I have a few whitespace patterns
-- here:
--   * require but skip
--   * optional but consume if present

annotation :: Parser DocNode
annotation = DocNode <$> (char '@' *> annotationName)
  where
    annotationName =
      choice [ command
             , string "dedent" >> return Dedent
             , string "footer" >> return Footer
             , string "indent" >> return Indent
             , try $ string "mappings" >> return Mappings -- must come before mapping
             , mapping
             , option
             , plugin
             ]

    command           = string "command" >> ws >> Command <$> ((:) <$> char ':' <*> (many1 (noneOf "\n")))

    mapping           = string "mapping" >> ws >> Mapping <$> mappingName
    mappingName       = word <* optional ws

    option            = string "option" >> ws >> Option <$> optionName <*> optionType <*> optionDefault
    optionName        = many1 (alphaNum <|> char ':') <* ws <?> "option name"
    optionType        = many1 alphaNum <* ws <?> "option type"
    optionDefault     = (optionMaybe $ word) <?> "option default value"

    plugin            = string "plugin" >> ws >> Plugin <$> pluginName <*> plugInDescription
    pluginName        = many1 alphaNum <* ws
    plugInDescription = manyTill anyChar (newline <|> (eof >> return EOF))

-- | Parses a translation unit (file contents) into an AST.
unit :: Parser [Node]
unit = do
  nodes <- many docComment
  eof
  return nodes

parse :: String -> IO [Node]
parse fileName = parseFromFile unit fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

-- | To facilitate quick testing in the console.
-- import Parse (p)
-- p "test"
p = runParser unit () "(eval)"
