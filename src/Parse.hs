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
          -- ...everything else
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

data Annotation = Plugin String String -- name desc
                | Indent
                | Dedent
                | Command String -- example
                | Footer
                | Mappings
                | Mapping String -- name
                | Option String String (Maybe String) -- name type optional-default
  deriving (Eq, Show)

-- These cause type errors unless used...
-- blockquote    = string ">" >> return Blockquote
-- commentStart  = string "\"" >> return CommentStart
docBlockStart = (string "\"\"" <* (optional whitespace)) >> return DocBlockStart
-- listItem      = string "-" >> return ListItem
newline       = char '\n' >> return Newline
whitespace    = Whitespace <$> many1 (oneOf " \t")

docComment :: Parser Node
docComment = docBlockStart *> (DocComment <$> many1 docNode)

docNode :: Parser DocNode
docNode = choice [ annotation
                 , heading
                 ]

heading :: Parser DocNode
heading = Heading <$> (char '#' >> (optional whitespace) *> (manyTill anyChar (newline <|> (eof >> return EOF))))
-- TODO: probably want to swallow the newline here; make it implicit
-- (and any trailing whitespace)

-- | Match a "word" of non-whitespace characters.
word = many1 (noneOf " \n\t")

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

    command = string "command" >> optional whitespace >> Command <$> ((:) <$> char ':' <*> (many1 (noneOf "\n")))

    mapping = string "mapping" >> optional whitespace >> Mapping <$> mappingName
    mappingName = word <* whitespace

    option = string "option" >> optional whitespace >> Option <$> optionName <*> optionType <*> optionDefault
    optionName = many1 (alphaNum <|> char ':') <* whitespace <?> "option name"
    optionType = many1 alphaNum <* whitespace <?> "option type"
    optionDefault = (optionMaybe $ word) <?> "option default value"

    plugin = string "plugin" >> optional whitespace >> Plugin <$> pluginName <*> plugInDescription
    pluginName = many1 alphaNum <* whitespace
    plugInDescription = manyTill anyChar (newline <|> (eof >> return EOF))

-- | Parses a translation unit (file contents) into an AST.
unit :: Parser [Node]
unit = (optional whitespace) *> (many docComment) <* eof

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
