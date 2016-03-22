{-# LANGUAGE FlexibleContexts #-}

module Docvim.Parse (p, parse) where

import Control.Applicative ( (*>)
                           , (<$)
                           , (<$>)
                           , (<*)
                           , (<*>)
                           )
import Data.List (intercalate)
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
                   , option
                   , optionMaybe
                   , optional
                   , parseTest
                   , runParser
                   , sepBy
                   , sepEndBy
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

data Unit = Unit [Node] deriving (Eq, Show)

-- Note that VimL can contain a DocComment almost anywhere, so will
-- probably want to make DocComment into a real island parser, with the
-- VimL parser being the primary parser. Won't attach VimL info to
-- DocComment nodes during the parse; will likely need a separate pass of the
-- AST after that.
data Node = FunctionDeclaration { functionBang :: Bool
                                , functionName :: String
                                , functionArguments :: ArgumentList
                                , functionAttributes :: [String]
                                }
          | DocNode [Annotation]
          | Heading String
  deriving (Eq)

-- The VimScript (VimL) grammar is embodied in the implementation of
-- https://github.com/vim/vim/blob/master/src/eval.c; there is no formal
-- specification for it, and there are many ambiguities that can only be
-- resolved at runtime. We aim to parse a loose subset.
-- data VimL = VimL State
-- data Statement = Statement String
-- data FunctionDeclaration = FunctionDeclaration Name FunctionBody (Maybe String)
-- data FunctionBody = FunctionBody [Statement]
-- Q: should i be using record syntax here?

-- TODO: deal with bar |
--       note that `function X() |` does not work, and `endf` must be on a line
--       of its own too (not a syntax error to do `| endf`, but it doesn't work
--       , so you need to add another `endf`, which will blow up at runtime.
-- TODO: validate name = CapitalLetter or s:foo or auto#loaded

instance Show Node where
  show (FunctionDeclaration bang name arguments _) =  keyword
                                                   ++ " "
                                                   ++ name
                                                   ++ show arguments
    where
      keyword | bang == True = "function!"
              | otherwise    = "function"
  show (Heading a) = show a
  show (DocNode a) = "not yet implemented"

data ArgumentList = ArgumentList [Argument]
  deriving (Eq)

instance Show ArgumentList where
  show (ArgumentList arguments) = "(" ++ intercalate ", " argStrings ++ ")"
    where
      argStrings = map show arguments

data Argument = Argument String
  deriving (Eq)

instance Show Argument where
  show (Argument argument) = argument

-- | Given a `description` like "fu[nction]", returns a parser that matches
-- "fu", "fun", "func", "funct", "functi", "functio" and "function".
--
-- Beware, may explode at runtime if passed an invalid `description`, due to the
-- use of `init`.
--
-- Requires the FlexibleContexts extension, for reasons that I don't yet fully
-- understand.
command description =   try (string prefix >> remainder rest)
                    <?> prefix ++ rest
  where prefix           = takeWhile (\c -> c /= '[') description
        rest             = init (snd (splitAt (1 + (length prefix)) description))
        remainder [r]    = optional (char r)
        remainder (r:rs) = optional (char r >> remainder rs)

function =   FunctionDeclaration
         <$> (fu *> bang <* wsc)
         <*> (name <* optional wsc)
         <*> arguments
         <*> (attributes <* optional wsc)
         <* (newline >> optional ws >> endf)
  where
    fu = command "fu[nction]"
    name = many1 alphaNum <* optional wsc
    bang = option False (True <$ char '!')
    arguments =  (char '(' >> optional wsc)
              *> (ArgumentList <$> argument `sepBy` (char ',' >> optional wsc))
              <* (optional wsc >> char ')' >> optional wsc)
    argument = Argument <$> many1 alphaNum <* optional wsc
    attributes = (choice [string "abort", string "range", string "dict"]) `sepEndBy` wsc
    endf = command "endf[unction]"
    -- body = optional $ FunctionBody <$> string "body"

-- | Textual tokens recognized during parsing but not embedded in the AST.
data Token = Blockquote
           | CommentStart
           | DocBlockStart
           | ListItem
           | Newline
           | Whitespace String
           | EOF
  deriving (Eq, Show)

type Default = String
type Description = String
type Name = String
type Type = String
type Usage = String
data Annotation = Plugin Name Description
                | Function Name -- not sure if I will want more here
                | Indent
                | Dedent
                | Command Usage
                | Footer
                | Mappings
                | Mapping Name
                | Option Name Type (Maybe Default)
  deriving (Eq, Show)

-- These cause type errors unless used...
-- blockquote    = string ">" >> return Blockquote
-- commentStart  = string "\"" >> return CommentStart
docBlockStart = (string "\"\"" <* optional ws) >> return DocBlockStart
-- listItem = string "-" >> return ListItem
newline = Newline <$ char '\n'
ws = Whitespace <$> many1 (oneOf " \t")

-- Continuation-aware whitespace (\)
wsc = many1 $ choice [whitespace, continuation]
  where whitespace = oneOf " \t"
        continuation = try $ char '\n' >> many whitespace >> char '\\'

node :: Parser Node
node = do
  optional ws

  maybeDocBlock <- optionMaybe $ try docBlockStart
  case maybeDocBlock of
    Just DocBlockStart -> DocNode <$> many1 docNode
    -- TODO: parse the VimScript too and extract metadata from it to attach to
    -- DocComment node
    Nothing -> function

-- docNode :: Parser Node
docNode = annotation
-- docNode = choice [ annotation
--                  , heading
--                  ]

-- heading :: Parser Node
heading = Heading <$> (char '#' >> optional ws *> manyTill anyChar (newline <|> (eof >> return EOF)))
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

-- TODO: only allow these after "" and " at start of line
annotation :: Parser Annotation
annotation = char '@' *> annotationName
  where
    annotationName =
      choice [ command
             , string "dedent" >> return Dedent
             , try $ string "footer" >> return Footer -- must come before function
             , function
             , string "indent" >> return Indent
             , try $ string "mappings" >> return Mappings -- must come before mapping
             , mapping
             , option
             , plugin
             ]

    command           = string "command" >> ws >> Command <$> ((:) <$> char ':' <*> many1 (noneOf "\n"))

    function          = string "function" >> ws >> Function <$> word <* optional ws

    mapping           = string "mapping" >> ws >> Mapping <$> mappingName
    mappingName       = word <* optional ws

    option            = string "option" >> ws >> Option <$> optionName <*> optionType <*> optionDefault
    optionName        = many1 (alphaNum <|> char ':') <* ws <?> "option name"
    optionType        = many1 alphaNum <* ws <?> "option type"
    optionDefault     = optionMaybe word <?> "option default value"

    plugin            = string "plugin" >> ws >> Plugin <$> pluginName <*> plugInDescription
    pluginName        = many1 alphaNum <* ws
    plugInDescription = manyTill anyChar (newline <|> (eof >> return EOF))

-- | Parses a translation unit (file contents) into an AST.
unit :: Parser Unit
unit = Unit <$> many node <* eof

parse :: String -> IO Unit
parse fileName = parseFromFile unit fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

-- | To facilitate quick testing in the console.
-- import Parse (p)
-- p "test"
p = parseTest unit
