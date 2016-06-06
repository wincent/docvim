{-# LANGUAGE FlexibleContexts #-}

module Docvim.Parse ( parse
                    , rstrip
                    , strip
                    , unit
                    ) where

import Control.Applicative ( (*>)
                           , (<$)
                           , (<$>)
                           , (<*)
                           , (<*>)
                           , liftA
                           , liftA2
                           )
import Data.Char (toUpper)
import Data.List (groupBy, intercalate)
import Docvim.AST
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
-- TODO: custom error messages with <?>
import Text.Parsec ( (<|>)
                   , (<?>)
                   , ParseError
                   , choice
                   , digit
                   , lookAhead
                   , many
                   , many1
                   , manyTill
                   , notFollowedBy
                   , option
                   , optionMaybe
                   , optional
                   , parseTest
                   , satisfy
                   , sepBy
                   , sepBy1
                   , sepEndBy
                   , sepEndBy1
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
                                          , upper
                                          )

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
         <*> (skippable *> many node <* (optional ws >> endfunction))
  where
    fu         = command "fu[nction]"
    name       = choice [script, normal, autoloaded] <* optional wsc
    script     = liftA2 (++) (try $ string "s:") (many $ oneOf identifier)
    normal     = liftA2 (++) (many1 upper) (many $ oneOf identifier)
    autoloaded = do
      a <- many1 $ oneOf identifier
      b <- string "#"
      c <- sepBy1 (many1 $ oneOf identifier) (string "#")
      return $ a ++ b ++ intercalate "#" c
    identifier = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    arguments  =  (char '(' >> optional wsc)
               *> (ArgumentList <$> argument `sepBy` (char ',' >> optional wsc))
               <* (optional wsc >> char ')' >> optional wsc)
    argument   = Argument <$> (string "..." <|> many1 alphaNum) <* optional wsc
    attributes = choice [string "abort", string "range", string "dict"] `sepEndBy` wsc

-- Disambiguate `:endf[unction]` and `:endfo[r]`
endfunction =  lookAhead (string "endf" >> notFollowedBy (string "o"))
            >> command "endf[unction]"
            <* eos

lStatement =  lookAhead (char 'l')
           >> choice [ try (lookAhead (string "lw")) >> lwindow
                     , try (lookAhead (string "let")) >> letStatement
                     , lexpr
                     ]

lwindow = LwindowStatement <$> (lw *> height <* eos)
  where
    lw     = command "l[window]"
    height = optionMaybe (wsc *> number)
    number = liftA read (many1 digit)

lexpr = LexprStatement
      <$> (command "lex[pr]" *> bang <* wsc)
      <*> restOfLine

-- "let" is a reserved word in Haskell, so we call this "letStatement" instead.
letStatement =   LetStatement
    <$> (string "let" >> wsc >> lhs)
    <*> (optional wsc >> char '=' >> optional wsc *> rhs <* eos)
  where
    -- Kludge alert! Until we get a full expression parser, we use this crude
    -- thing.
    lhs = many1 $ noneOf "\"\n="
    rhs = many1 $ noneOf "\n"

unlet =   UnletStatement
      <$> (unl *> bang <* wsc)
      <*> word
      <*  eos
  where
    unl  = command "unl[et]"

quote = string "\"" <?> "quote"
commentStart  = quote <* (notFollowedBy quote >> optional ws)
docBlockStart = (string "\"\"" <* optional ws) <?> "\"\""

separator = Separator <$ (try (string "---") >> optional ws) <?> "wat"

fenced = fence >> newline >> Fenced <$> body
  where
    fence = try $ string "```" >> optional ws
    body = do
      lines <- manyTill line (try $ (commentStart <|> docBlockStart) >> optional ws >> fence)
      let indent = foldr countLeadingSpaces infinity lines
      return $ map (trimLeadingSpace indent) lines
      where
        -- Find minimum count of leading spaces.
        countLeadingSpaces line = min (length (takeWhile (' ' ==) line))
        trimLeadingSpace count = if count > 0
                                 then drop count
                                 else id
        infinity = maxBound :: Int
    line           = (commentStart' <|> docBlockStart') >> restOfLine <* newline
    commentStart'  = quote <* notFollowedBy quote
    docBlockStart' = string "\"\"" <?> "\"\""

blockquote =   lookAhead (char '>')
           >>  Blockquote
           <$> paragraph `sepBy1` blankLine
  where
    paragraph = Paragraph <$> body
    body = do
      first  <- firstLine
      rest   <- many otherLine
      -- Make every line end with whitespace.
      let nodes = concatMap appendWhitespace (first:rest)
      -- Collapse consecutive whitespace.
      let compressed = compress nodes
      -- Trim final whitespace.
      return ( if last compressed == Whitespace
               then init compressed
               else compressed )
    firstLine =  char '>'
              >> optional ws
              >> many1 (choice [phrasing, whitespace])
    otherLine =  try $ newline
              >> (commentStart <|> docBlockStart)
              >> firstLine
    blankLine =  try $ newline
              >> (commentStart <|> docBlockStart)
              >> many1 (try $ char '>'
              >> optional ws
              >> newline
              >> (commentStart <|> docBlockStart))

list =  lookAhead (char '-' >> notFollowedBy (char '-'))
     >> List
     <$> listItem `sepBy1` separator
  where
    -- Yes, this is a bit hideous.
    separator =  try $ newline
              >> (commentStart <|> docBlockStart)
              >> optional ws
              >> lookAhead (char '-')

listItem =  lookAhead (char '-' >> notFollowedBy (char '-'))
         >> ListItem
         <$> body
  where
    body = do
      first  <- firstLine
      rest   <- many otherLine
      -- Make every line end with whitespace.
      let nodes = concatMap appendWhitespace (first:rest)
      -- Collapse consecutive whitespace.
      let compressed = compress nodes
      -- Trim final whitespace.
      return ( if last compressed == Whitespace
               then init compressed
               else compressed )
    firstLine = char '-'
              >> optional ws
              >> many1 (choice [phrasing, whitespace])
    otherLine =  try $ newline
              >> (commentStart <|> docBlockStart)
              -- TODO ^ DRY this up?
              >> optional ws
              >> lookAhead (noneOf "-")
              >> many1 (choice [phrasing, whitespace])

-- | Newline (and slurps up following horizontal whitespace as well).
newline = (char '\n' >> optional ws) <|> eof
newlines =   many1 (char '\n' >> optional ws)
         <|> (eof >> return [()])

-- | Whitespace (specifically, horizontal whitespace: spaces and tabs).
ws = many1 (oneOf " \t")

-- | Continuation-aware whitespace (\).
wsc = many1 $ choice [whitespace, continuation]
  where
    whitespace   = oneOf " \t"
    continuation = try $ char '\n' >> ws >> char '\\'

-- TODO: string literals; some nasty lookahead might be required
comment = try
        $ quote
        >> notFollowedBy quote
        >> restOfLine
        >> skipMany (char '\n' >> optional ws)

-- | Optional bang suffix for VimL commands.
bang = option False (True <$ char '!')

-- | End-of-statement.
-- TODO: see `:h :bar` for a list of commands which see | as an arg instead of a
-- command separator.
eos = optional ws >> choice [bar, ws', skipMany1 comment]
  where
    bar = char '|' >> optional wsc
    ws' = newlines >> notFollowedBy wsc

node :: Parser Node
node =  choice [ docBlock
               , vimL
               ]
     <* optional skippable

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
                           , separator
                           , list
                           , blockquote
                           , fenced
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
      let nodes = concatMap appendWhitespace (first:rest)
      -- Collapse consecutive whitespace
      let compressed = compress nodes
      -- Trim final whitespace
      return ( if last compressed == Whitespace
               then init compressed
               else compressed )
    firstLine = many1 $ choice [phrasing, whitespace]
    otherLine =  try $ newline
              >> (commentStart <|> docBlockStart)
              >> optional ws
              >> notFollowedBy special
              >> firstLine

-- | Used in lookahead rules to make sure that we don't greedily consume special
-- tokens as if they were just phrasing content.
special :: Parser String
special = choice [ string "-" <* notFollowedBy (char '-')
                 , string ">"
                 , string "---"
                 , string "-" <* string "--"
                 , string "```"
                 , string "`" <* string "``"
                 , string "@"
                 , string "#"
                 ]

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
    hasBreakTag = elem BreakTag
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
-- whitespace inside "phrasing content" is significant so is preserved (in
-- normalized form) in the AST.
whitespace = Whitespace <$ ws

br = BreakTag <$ (try htmlTag <|> try xhtmlTag) <?> "<br />"
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
statement = choice [ lStatement
                   , unlet
                   , genericStatement
                   ]

-- | Generic VimL node parser to represent stuff that we haven't built out full parsing
-- for yet.
genericStatement = do
  -- Make sure we never recognize `endfunction` as a generic statement. This is
  -- necessary because we call `node` recursively inside `function` while
  -- parsing the function body. We must stop `node` from consuming
  -- `endfunction`, otherwise the `function` parse will fail to find it.
  notFollowedBy endfunction
  atoms <- sepEndBy1 word (optional wsc)
  eos
  return $ GenericStatement $ unwords atoms

-- | Remainder of the line up to but not including a newline.
-- Does not include any trailing whitespace.
restOfLine :: Parser String
restOfLine = do
  rest <- many (noneOf "\n")
  return $ rstrip rest

-- | Strip trailing and leading whitespace.
--
-- Not efficient, but chosen for readablility.
--
-- TODO: switch to Data.Text (http://stackoverflow.com/a/6270382/2103996) for
-- efficiency.
strip = lstrip . rstrip

-- | Strip leading (left) whitespace.
lstrip = dropWhile (`elem` " \n\t")

-- | Strip trailing (right) whitespace.
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

-- TODO: only allow these after "" and " at start of line
annotation :: Parser Node
annotation = char '@' *> annotationName
  where
    annotationName =
      choice [ try $ string "commands" >> pure CommandsAnnotation -- must come before function
             , command
             , string "dedent" >> pure DedentAnnotation
             , try $ string "footer" >> pure FooterAnnotation -- must come before function
             , try $ string "functions" >> pure FunctionsAnnotation -- must come before function
             , function
             , string "indent" >> pure IndentAnnotation
             , try $ string "mappings" >> pure MappingsAnnotation -- must come before mapping
             , mapping
             , try $ string "options" >> pure OptionsAnnotation -- must come before option
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
    -- BUG: this is not optional, it would seem (see tests/fixtures/vim/options.vim)
    optionDefault     = optionMaybe word <?> "option default value"

    plugin            = string "plugin" >> ws >> PluginAnnotation <$> pluginName <*> plugInDescription
    pluginName        = many1 alphaNum <* ws
    plugInDescription = restOfLine

-- | Parses a translation unit (file contents) into an AST.
unit :: Parser Node
unit =   Unit
     <$> (skippable >> many node)
     <*  eof

skippable = many $ choice [ comment
                          , skipMany1 ws
                          , skipMany1 (char '\n')
                          ]

parse :: String -> IO Node
parse fileName = parseFromFile unit fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure
