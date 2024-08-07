{-# LANGUAGE FlexibleContexts #-}

module Text.Docvim.Parse ( parse
                         , rstrip
                         , strip
                         , unit
                         ) where

import Control.Applicative hiding ((<|>), many, optional)
import Data.Char
import Data.List (groupBy, intercalate)
import System.Exit
import System.IO
import Text.Docvim.AST
import Text.Parsec hiding (newline, parse)
import Text.Parsec.String

-- | Given a `description` like "fu[nction]", returns a parser that matches
-- "fu", "fun", "func", "funct", "functi", "functio" and "function".
--
-- Beware, may explode at runtime if passed an invalid `description`, due to the
-- use of `init`.
--
-- Requires the FlexibleContexts extension, for reasons that I don't yet fully
-- understand.
command :: String -> Parser ()
command description =   try (string prefix >> remainder rest)
                    <?> prefix ++ rest
  where prefix           = takeWhile (/= '[') description
        rest             = init (snd (splitAt (1 + length prefix) description))
        remainder [r]    = optional (char r)
        remainder (r:rs) = optional (char r >> remainder rs)
        remainder []     = error "Unexpected empty remainder"

function :: Parser Node
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
    argument   = Argument <$> (string "..." <|> many1 (oneOf identifier)) <* optional wsc
    attributes = choice [string "abort", string "range", string "dict"] `sepEndBy` wsc

-- Disambiguate `:endf[unction]` and `:endfo[r]`
endfunction :: Parser ()
endfunction =  lookAhead (string "endf" >> notFollowedBy (string "o"))
            >> command "endf[unction]"
            <* eos

lStatement :: Parser Node
lStatement =  lookAhead (char 'l')
           >> choice [ try (lookAhead (string "lw")) >> lwindow
                     , try (lookAhead (string "let")) >> letStatement
                     , lexpr
                     ]

lwindow :: Parser Node
lwindow = LwindowStatement <$> (lw *> height <* eos)
  where
    lw     = command "l[window]"
    height = optionMaybe (wsc *> number)
    number = liftA read (many1 digit)

lexpr :: Parser Node
lexpr = LexprStatement
      <$> (command "lex[pr]" *> bang <* wsc)
      <*> restOfLine

-- "let" is a reserved word in Haskell, so we call this "letStatement" instead.
letStatement :: Parser Node
letStatement =   LetStatement
    <$> (string "let" >> wsc >> lhs)
    <*> (optional wsc >> char '=' >> optional wsc *> rhs <* eos)
  where
    -- Kludge alert! Until we get a full expression parser, we use this crude
    -- thing.
    lhs = many1 $ noneOf "\"\n="
    rhs = many1 $ noneOf "\n"

unlet :: Parser Node
unlet =   UnletStatement
      <$> (unl *> bang <* wsc)
      <*> word
      <*  eos
  where
    unl  = command "unl[et]"

quote :: Parser String
quote = string "\"" <?> "quote"

commentStart :: Parser String
commentStart  = quote <* (notFollowedBy quote >> optional ws)

docBlockStart :: Parser String
docBlockStart = (string "\"\"" <* optional ws) <?> "\"\""

separator :: Parser Node
separator = Separator <$ (try (string "---") >> optional ws) <?> "wat"

fenced :: Parser Node
fenced = fence >> newline >> Fenced <$> body
  where
    fence = try $ string "```" >> optional ws
    body = do
      lines' <- manyTill line (try $ (commentStart <|> docBlockStart) >> optional ws >> fence)
      let indent = foldr countLeadingSpaces infinity lines'
      return $ map (trimLeadingSpace indent) lines'
      where
        -- Find minimum count of leading spaces.
        countLeadingSpaces line' = min (length (takeWhile (' ' ==) line'))
        trimLeadingSpace count' = if count' > 0
                                 then drop count'
                                 else id
        infinity = maxBound :: Int
    line           = (commentStart' <|> docBlockStart') >> restOfLine <* newline
    commentStart'  = quote <* notFollowedBy quote
    docBlockStart' = string "\"\"" <?> "\"\""

blockquote :: Parser Node
blockquote =   lookAhead (char '>')
           >>  Blockquote
           <$> paragraph' `sepBy1` blankLine
  where
    paragraph' = Paragraph <$> body
    body = paragraphBody firstLine otherLine
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

list :: Parser Node
list =  lookAhead (char '-' >> notFollowedBy (char '-'))
     >> List
     <$> listItem `sepBy1` separator'
  where
    -- Yes, this is a bit hideous.
    separator' =  try $ newline
               >> (commentStart <|> docBlockStart)
               >> optional ws
               >> lookAhead (char '-')

listItem :: Parser Node
listItem =  lookAhead (char '-' >> notFollowedBy (char '-'))
         >> ListItem
         <$> body
  where
    body = paragraphBody firstLine otherLine
    firstLine = char '-' >> optional ws >> many1 (choice [phrasing, whitespace])
    otherLine =  try $ newline
              >> (commentStart <|> docBlockStart)
              -- TODO ^ DRY this up?
              >> optional ws
              >> lookAhead (noneOf "-")
              >> many1 (choice [phrasing, whitespace])

-- | Newline (and slurps up following horizontal whitespace as well).
newline :: Parser ()
newline = (char '\n' >> optional ws) <|> eof

newlines :: Parser [()]
newlines =   many1 (char '\n' >> optional ws)
         <|> (eof >> return [()])

-- | Whitespace (specifically, horizontal whitespace: spaces and tabs).
ws :: Parser String
ws = many1 (oneOf " \t")

-- | Continuation-aware whitespace (\).
wsc :: Parser String
wsc = many1 $ choice [whitespace', continuation]
  where
    whitespace'   = oneOf " \t"
    continuation = try $ char '\n' >> ws >> char '\\'

-- TODO: string literals; some nasty lookahead might be required
comment :: Parser ()
comment = try
        $ quote
        >> notFollowedBy quote
        >> restOfLine
        >> skipMany (char '\n' >> optional ws)

-- | Optional bang suffix for VimL commands.
bang :: Parser Bool
bang = option False (True <$ char '!')

-- | End-of-statement.
-- TODO: see `:h :bar` for a list of commands which see | as an arg instead of a
-- command separator.
eos :: Parser ()
eos = optional ws >> choice [bar, ws', skipMany1 comment]
  where
    bar = char '|' >> optional wsc
    ws' = newlines >> notFollowedBy wsc

node :: Parser Node
node =  choice [ docBlock
               , vimL
               ]
     <* optional skippable

docBlock :: Parser Node
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

paragraph :: Parser Node
paragraph = Paragraph <$> body
  where
    body = paragraphBody firstLine otherLine
    firstLine = many1 $ choice [phrasing, whitespace]
    otherLine =  try $ newline
              >> (commentStart <|> docBlockStart)
              >> optional ws
              >> notFollowedBy special
              >> firstLine

paragraphBody :: Parser [Node] -> Parser [Node] -> Parser [Node]
paragraphBody firstLine otherLine = do
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

phrasing :: Parser Node
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
plaintext :: Parser Node
plaintext = Plaintext <$> wordChars
  where
    wordChars = many1 $ choice [ try $ char '<' <* notFollowedBy (caseString' "br")
                               , noneOf " \n\t<|`"
                               ]

-- | Case-insensitive char match.
--
-- Based on `caseChar` function in:
-- https://hackage.haskell.org/package/hsemail-1.3/docs/Text-ParserCombinators-Parsec-Rfc2234.html
char' :: Char -> Parser Char
char' c = satisfy $ \x -> toUpper x == toUpper c

-- | Case-insensitive string match.
--
-- Based on `caseString` function in:
-- https://hackage.haskell.org/package/hsemail-1.3/docs/Text-ParserCombinators-Parsec-Rfc2234.html
caseString' :: String -> Parser String
caseString' s = mapM_ char' s >> pure s <?> s

-- | Tokenized whitespace.
--
-- Most whitespace is insignificant and gets omitted from the AST, but
-- whitespace inside "phrasing content" is significant so is preserved (in
-- normalized form) in the AST.
whitespace :: Parser Node
whitespace = Whitespace <$ ws

br :: Parser Node
br = BreakTag <$ (try htmlTag <|> try xhtmlTag) <?> "<br />"
  where
    htmlTag = caseString' "<br>"
    xhtmlTag = caseString' "<br" >> optional ws >> string "/>"

link :: Parser Node
link = Link <$> (bar *> linkText <* bar)
  where
    bar      = char '|'
    linkText = many1 $ noneOf " \t\n|"

code :: Parser Node
code = Code <$> (backtick *> codeText <* backtick)
  where
    backtick = char '`'
    codeText = many $ noneOf "\n`"

linkTargets :: Parser Node
linkTargets = LinkTargets <$> many1 (star *> target <* (star >> optional ws))
  where
    star = char '*'
    target = many1 $ noneOf " \t\n*"

vimL :: Parser Node
vimL = choice [ block
              , statement
              ]

block :: Parser Node
block = choice [ function ]

statement :: Parser Node
statement = choice [ lStatement
                   , unlet
                   , genericStatement
                   ]

-- | Generic VimL node parser to represent stuff that we haven't built out full parsing
-- for yet.
genericStatement :: Parser Node
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
strip :: String -> String
strip = lstrip . rstrip

-- | Strip leading (left) whitespace.
lstrip :: String -> String
lstrip = dropWhile (`elem` " \n\t")

-- | Strip trailing (right) whitespace.
rstrip :: String -> String
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
word :: Parser String
word = many1 (noneOf " \n\t")

-- TODO: only allow these after "" and " at start of line
annotation :: Parser Node
annotation = char '@' *> annotationName
  where
    annotationName =
      choice [ try $ string "commands" >> pure CommandsAnnotation -- must come before function
             , command'
             , string "dedent" >> pure DedentAnnotation
             , try $ string "footer" >> pure FooterAnnotation -- must come before function'
             , try $ string "functions" >> pure FunctionsAnnotation -- must come before function'
             , function'
             , string "header" >> pure HeaderAnnotation
             , try $ string "indent" >> pure IndentAnnotation -- must come before image'
             , image'
             , try $ string "mappings" >> pure MappingsAnnotation -- must come before mapping
             , mapping
             , try $ string "options" >> pure OptionsAnnotation -- must come before option'
             , option'
             , plugin
             ]

    command'          = string "command" >> ws >> CommandAnnotation <$> commandName <*> commandParameters
    commandName       = char ':' *> many1 (alphaNum <|> char '!') <* optional ws
    commandParameters = optionMaybe $ many1 (noneOf "\n")

    function'         = string "function" >> ws >> FunctionAnnotation <$> word <* optional ws

    image'            = string "image" >> ws >> ImageAnnotation <$> imageSource <*> imageAlignment
    imageSource       = word <* optional ws
    imageAlignment    = optionMaybe $ many1 (noneOf "\n")

    mapping           = string "mapping" >> ws >> MappingAnnotation <$> mappingName
    mappingName       = word <* optional ws

    option'           = string "option" >> ws >> OptionAnnotation <$> optionName <*> optionType <*> optionDefault
    optionName        = many1 (alphaNum <|> char ':') <* ws <?> "option name"
    optionType        = many1 alphaNum <* optional ws <?> "option type"
    optionDefault     = optionMaybe $ many1 (noneOf "\n")

    plugin            = string "plugin" >> ws >> PluginAnnotation <$> pluginName <*> plugInDescription
    pluginName        = many1 (alphaNum <|> char '-') <* ws
    plugInDescription = restOfLine

-- | Parses a translation unit (file contents) into an AST.
unit :: Parser Node
unit =   Unit
     <$> (skippable >> many node)
     <*  eof

skippable :: Parser [()]
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
