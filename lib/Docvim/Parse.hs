{-# LANGUAGE FlexibleContexts #-}

module Docvim.Parse ( p
                    , parse
                    , parseUnit
                    , pp
                    ) where

import Control.Applicative ( (*>)
                           , (<$)
                           , (<$>)
                           , (<*)
                           , (<*>)
                           )
import Data.Char (toUpper)
import Data.List (groupBy, intercalate)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
-- TODO: custom error messages with <?>
import Text.Parsec ( (<|>)
                   , (<?>)
                   , ParseError
                   , choice
                   , lookAhead
                   , many
                   , many1
                   , manyTill
                   , notFollowedBy
                   , option
                   , optionMaybe
                   , optional
                   , parseTest
                   , runParser
                   , satisfy
                   , sepBy
                   , sepEndBy
                   , skipMany
                   , skipMany1
                   , try
                   , unexpected
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

-- might want to pull this into a separate, test-only module
import Text.Show.Pretty (ppShow)

data Unit = Unit [Node] deriving (Eq, Show)

-- Note that VimL can contain a DocComment almost anywhere, so will
-- probably want to make DocComment into a real island parser, with the
-- VimL parser being the primary parser. Won't attach VimL info to
-- DocComment nodes during the parse; will likely need a separate pass of the
-- AST after that.
data Node
          -- VimL nodes
          = FunctionDeclaration { functionBang :: Bool
                                , functionName :: String
                                , functionArguments :: ArgumentList
                                , functionAttributes :: [String]
                                , functionBody :: [Node]
                                }
          | LetStatement { letLexpr :: String
                         , letValue :: String
                         }
          | UnletStatement { unletBang :: Bool
                           , unletBody :: String
                           }

          -- Docvim nodes: "block-level" elements
          | DocBlock [Node]
          | Paragraph [Node]
          | LinkTargets [String]
          | ListItem [Node]
          | Blockquote [String]

          -- Docvim nodes: "phrasing content" elements
          | Plaintext String
          | BreakTag
          | Link String
          | Code String
          | Whitespace

          -- Docvim nodes: annotations
          | PluginAnnotation Name Description
          | FunctionAnnotation Name -- not sure if I will want more here
          | IndentAnnotation
          | DedentAnnotation
          | CommandAnnotation Usage
          | FooterAnnotation
          | MappingsAnnotation
          | MappingAnnotation Name
          | OptionAnnotation Name Type (Maybe Default)
          | HeadingAnnotation String
          | SubheadingAnnotation String
  deriving (Eq, Show)

-- The VimScript (VimL) grammar is embodied in the implementation of
-- https://github.com/vim/vim/blob/master/src/eval.c; there is no formal
-- specification for it, and there are many ambiguities that can only be
-- resolved at runtime. We aim to parse a loose subset.

-- TODO: deal with bar |
--       note that `function X() |` does not work, and `endf` must be on a line
--       of its own too (not a syntax error to do `| endf`, but it doesn't work
--       , so you need to add another `endf`, which will blow up at runtime.
-- TODO: validate name = CapitalLetter or s:foo or auto#loaded

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
  where prefix           = takeWhile (/= '[') description
        rest             = init (snd (splitAt (1 + length prefix) description))
        remainder [r]    = optional (char r)
        remainder (r:rs) = optional (char r >> remainder rs)

function =   FunctionDeclaration
         <$> (fu *> bang <* wsc)
         <*> (name <* optional wsc)
         <*> arguments
         <*> (attributes <* optional wsc)
         <*> (newlines *> many node <* (optional ws >> endf))
  where
    fu         = command "fu[nction]"
    name       = many1 alphaNum <* optional wsc
    arguments  =  (char '(' >> optional wsc)
               *> (ArgumentList <$> argument `sepBy` (char ',' >> optional wsc))
               <* (optional wsc >> char ')' >> optional wsc)
    argument   = Argument <$> many1 alphaNum <* optional wsc
    attributes = choice [string "abort", string "range", string "dict"] `sepEndBy` wsc
    endf       = command "endf[unction]" <* eos

-- "let" is a reserved word in Haskell, so we call this "letStatement" instead.
letStatement =   LetStatement
    <$> (string "let" >> wsc >> word')
    <*> (optional wsc >> char '=' >> optional wsc *> word' <* eos)
  where
    word' = many1 $ noneOf " \n\t="

unlet =   UnletStatement
      <$> (unl *> bang <* wsc)
      <*> word
      <*  eos
  where
    unl  = command "unl[et]"

type Default = String
type Description = String
type Name = String
type Type = String
type Usage = String

quote = string "\"" <?> "quote"
commentStart  = quote <* (notFollowedBy quote >> optional ws)
docBlockStart = (string "\"\"" <* optional ws) <?> "\"\""

-- TODO: allow blank lines within blockquote
blockquote = lookAhead (char '>') >> Blockquote <$> body
  where
    body = do
      first  <- firstLine
      rest   <- many otherLine
      return (first:rest)
    firstLine = char '>' >> optional ws >> restOfLine
    otherLine =  try $ newline
              >> optional ws
              >> (commentStart <|> docBlockStart)
              >> firstLine

listItem = lookAhead (char '-') >> ListItem <$> body
  where
    body = do
      first  <- firstLine
      rest   <- many otherLine
      -- Make every line end with whitespace.
      let nodes = concat $ map appendWhitespace (first:rest)
      -- Collapse consecutive whitespace.
      let compressed = compress nodes
      -- Trim final whitespace.
      return $ ( if last compressed == Whitespace
                 then init compressed
                 else compressed )
    firstLine = char '-'
              >> optional ws
              >> (many1 $ choice [phrasing, whitespace])
    otherLine =  try $ newline
              >> optional ws
              >> (commentStart <|> docBlockStart)
              -- TODO ^ DRY this up?
              >> optional ws
              >> lookAhead (noneOf "-")
              >> (many1 $ choice [phrasing, whitespace])

-- | Newline (and slurps up following horizontal whitespace as well).
newline = (char '\n' >> optional ws) <|> eof
newlines = many1 (char '\n' >> optional ws)

-- | Whitespace (specifically, horizontal whitespace: spaces and tabs).
ws = many1 (oneOf " \t")

-- | Continuation-aware whitespace (\).
wsc = many1 $ choice [whitespace, continuation]
  where
    whitespace   = oneOf " \t"
    continuation = try $ char '\n' >> ws >> char '\\'

-- TODO: string literals; some nasty lookahead might be required
comment = try $ quote >> notFollowedBy quote >> restOfLine >> optional newline

-- | Optional bang suffix for VimL commands.
bang = option False (True <$ char '!')

-- | End-of-statement.
-- TODO: see `:h :bar` for a list of commands which see | as an arg instead of a
-- command separator.
eos = optional ws >> choice [bar, ws', skipMany comment]
  where
    bar = char '|' >> optional wsc
    ws' = newlines >> notFollowedBy wsc

node :: Parser Node
node =  optional comment
     >> choice [ docBlock
               , vimL
               ]

docBlock = lookAhead docBlockStart
         >> (DocBlock <$> many1 blockElement)
         <* trailingBlankCommentLines
  where
    blockElement =  try $ start
                 >> skipMany emptyLines
                 *> choice [ annotation
                           , try subheading -- must come before heading
                           , heading
                           , linkTargets
                           , listItem
                           , blockquote
                           , paragraph -- must come last
                           ]
                 <* next
    start = try docBlockStart <|> commentStart
    emptyLines = try $ newline >> start
    next = optional ws >> newline
    trailingBlankCommentLines = skipMany $ start >> newline

paragraph = Paragraph <$> body
  where
    body = do
      first <- firstLine
      rest <- many otherLine
      -- Make every line end with whitespace
      let nodes = concat $ map appendWhitespace (first:rest)
      -- Collapse consecutive whitespace
      let compressed = compress nodes
      -- Trim final whitespace
      return $ ( if last compressed == Whitespace
                 then init compressed
                 else compressed )
    firstLine = many1 $ choice [phrasing, whitespace]
    otherLine =  try $ newline
              >> optional ws
              >> (commentStart <|> docBlockStart)
              >> optional ws
              -- maybe lookAhead (noneOf "->") etc
              >> firstLine
phrasing = choice [ br
                  , link
                  , code
                  , plaintext
                  ]

-- | Appends a Whitespace token to a list of nodes.
appendWhitespace :: [Node] -> [Node]
appendWhitespace xs = xs ++ [Whitespace]

-- | Compress whitespace.
-- Consecutive Whitespace tokens are replaced with a single token.
-- If a run of whitespace includes a BreakTag, the run is replaced with the
-- BreakTag.
compress :: [Node] -> [Node]
compress = map prioritizeBreakTag . group
  where
    group                    = groupBy fn
    fn BreakTag Whitespace   = True
    fn Whitespace BreakTag   = True
    fn Whitespace Whitespace = True
    fn _ _                   = False
    prioritizeBreakTag xs = if hasBreakTag xs
                            then BreakTag
                            else head xs
    hasBreakTag = any (\n -> n == BreakTag)
-- similar to "word"... might end up replacing "word" later on...
-- something more sophisticated here with satisfy?
plaintext = Plaintext <$> wordChars
  where
    wordChars = many1 $ choice [ try $ char '<' <* notFollowedBy (string' "br")
                               , noneOf " \n\t<|`"
                               ]

-- | Case-insensitive char match.
--
-- Based on `caseChar` function in:
-- https://hackage.haskell.org/package/hsemail-1.3/docs/Text-ParserCombinators-Parsec-Rfc2234.html
char' c = satisfy $ \x -> toUpper x == toUpper c

-- | Case-insensitive string match.
--
-- Based on `caseString` function in:
-- https://hackage.haskell.org/package/hsemail-1.3/docs/Text-ParserCombinators-Parsec-Rfc2234.html
string' s = mapM_ char' s >> pure s <?> s

-- | Tokenized whitespace.
--
-- Most whitespace is insignificant and gets omitted from the AST, but
-- whitespace inside "phrasing content" is significant so is preserved i nthe
-- AST,  at least in normalized form.
whitespace = Whitespace <$ ws

br = BreakTag <$ (try htmlTag <|> xhtmlTag) <?> "<br />"
  where
    htmlTag = string' "<br>"
    xhtmlTag = string' "<br" >> optional ws >> string "/>"

link = Link <$> (bar *> linkText <* bar)
  where
    bar      = char '|'
    linkText = many1 $ noneOf " \t\n|"

code = Code <$> (backtick *> codeText <* backtick)
  where
    backtick = char '`'
    codeText = many $ noneOf "\n`"

-- TODO: record this in symbol table similar to
-- https://github.com/wincent/docvim/blob/js/src/SymbolVisitor.js
-- (probably want to make this a post-processing step?)
linkTargets = LinkTargets <$> many1 (star *> target <* (star >> optional ws))
  where
    star = char '*'
    target = many1 $ noneOf " \t\n*"

vimL = choice [ block
              , statement
              ]

block = choice [ function ]
statement = choice [ letStatement
                   , unlet
                   ]

-- | Remainder of the line up to but not including a newline.
-- Does not include any trailing whitespace.
restOfLine :: Parser String
restOfLine = do
  rest <- many1 (noneOf "\n")
  return $ strip rest
  where strip = lstrip . rstrip
        lstrip = dropWhile (`elem` " \t")
        rstrip = reverse . lstrip . reverse

heading :: Parser Node
heading =  char '#'
        >> notFollowedBy (char '#')
        >> optional ws
        >> HeadingAnnotation <$> restOfLine

subheading :: Parser Node
subheading =  string "##"
           >> optional ws
           >> SubheadingAnnotation <$> restOfLine

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
annotation :: Parser Node
annotation = char '@' *> annotationName
  where
    annotationName =
      choice [ command
             , string "dedent" >> pure DedentAnnotation
             , try $ string "footer" >> pure FooterAnnotation -- must come before function
             , function
             , string "indent" >> pure IndentAnnotation
             , try $ string "mappings" >> pure MappingsAnnotation -- must come before mapping
             , mapping
             , option
             , plugin
             ]

    command           = string "command" >> ws >> CommandAnnotation <$> ((:) <$> char ':' <*> many1 (noneOf "\n"))

    function          = string "function" >> ws >> FunctionAnnotation <$> word <* optional ws

    mapping           = string "mapping" >> ws >> MappingAnnotation <$> mappingName
    mappingName       = word <* optional ws

    option            = string "option" >> ws >> OptionAnnotation <$> optionName <*> optionType <*> optionDefault
    optionName        = many1 (alphaNum <|> char ':') <* ws <?> "option name"
    optionType        = many1 alphaNum <* ws <?> "option type"
    optionDefault     = optionMaybe word <?> "option default value"

    plugin            = string "plugin" >> ws >> PluginAnnotation <$> pluginName <*> plugInDescription
    pluginName        = many1 alphaNum <* ws
    plugInDescription = restOfLine

-- | Parses a translation unit (file contents) into an AST.
unit :: Parser Unit
unit =   Unit
     <$> (skippable >> many node)
     <*  eof
  where
    skippable = many $ choice [ comment
                              , skipMany1 ws
                              , skipMany1 (char '\n')
                              ]

parse :: String -> IO Unit
parse fileName = parseFromFile unit fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

-- | To facilitate unit-testing.
parseUnit :: String -> Either ParseError Unit
parseUnit = runParser unit () "(eval)"

-- | For unit testing.
p :: String -> String
p input = case parseUnit input of
            Left error -> show error
            Right ast -> ppShow ast

-- | To facilitate quick testing in the console.
-- import Parse (pp)
-- pp "unlet g:var"
-- pp :: String -> IO ()
pp input = putStrLn $ p input
