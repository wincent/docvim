module Docvim.Printer.Vim (vimHelp) where

import Control.Arrow ((***))
import Control.Monad (join)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isSpace, toLower, toUpper)
import Data.List (intercalate, isSuffixOf, span, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import Docvim.AST
import Docvim.Parse (rstrip)
import Docvim.Visitor.Plugin (getPluginName)
import Docvim.Visitor.Symbol (getSymbols)

-- TODO: add indentation here (using local, or just stick it in Context)

-- Instead of building up a [Char], we build up a list of operations, which
-- allows us a mechanism of implementing rollback and therefore hard-wrapping
-- (eg. append whitespace " ", then on next node, realize that we will exceed
-- line length limit, so rollback the " " and instead append "\n" etc).
data Operation = Append String
               | Delete Int -- unconditional delete count of Char
               | Slurp String -- delete string if present
data Metadata = Metadata { symbols :: [String]
                         , pluginName :: Maybe String
                         }
data Context = Context { lineBreak :: String
                       , partialLine :: String
                       }
type Env = ReaderT Metadata (State Context) [Operation]

textwidth :: Int
textwidth = 78

vimHelp :: Node -> String
vimHelp n = suppressTrailingWhitespace output ++ "\n"
  where metadata = Metadata (getSymbols n) (getPluginName n)
        context = Context defaultLineBreak ""
        operations = evalState (runReaderT (node n) metadata) context
        output = foldl reduce "" operations
        reduce acc (Append atom) = acc ++ atom
        reduce acc (Delete count) = take (length acc - count) acc
        reduce acc (Slurp atom) = if atom `isSuffixOf` acc
                                  then take (length acc - length atom) acc
                                  else acc
        suppressTrailingWhitespace str = rstrip $ intercalate "\n" (map rstrip (splitOn "\n" str))

-- | Helper function that appends and updates `partialLine` context,
-- hard-wrapping if necessary to remain under `textwidth`.
append :: String -> Env
append string = append' string textwidth

-- | Helper function that appends and updates `partialLine` context
-- uncontitionally (no hard-wrapping).
appendNoWrap :: String -> Env
appendNoWrap string = append' string (maxBound :: Int)

append' :: String -> Int -> Env
append' string width = do
  context <- get
  -- TODO obviously tidy this up
  let (ops, line) = if renderedWidth (partialLine context) + renderedWidth leading >= width
                    then ( [ Delete (length $ snd $ hardwrap $ partialLine context)
                           , Slurp " "
                           , Append (lineBreak context)
                           , Append (snd $ hardwrap $ partialLine context)
                           , Append string
                           ]
                         , lineBreak context ++ snd (hardwrap $ partialLine context) ++ string
                         )
                    else ([Append string], partialLine context ++ string)
  put (Context (lineBreak context) (end line))
  return ops
  where
    leading = takeWhile (/= '\n') string
    trailing str = length $ takeWhile isSpace (reverse str)
    end l = reverse $ takeWhile (/= '\n') (reverse l)

-- http://stackoverflow.com/a/9723976/2103996
mapTuple = join (***)

-- Given a string, hardwraps it into two parts by splitting it at the rightmost
-- whitespace.
hardwrap :: String -> (String, String)
hardwrap str = swap $ mapTuple reverse split
  where
    split = break isSpace (reverse str)

-- Helper function that deletes `count` elements from the end of the
--`partialLine` context.
delete :: Int -> Env
delete count = do
  context <- get
  put (Context (lineBreak context) (partial context))
  return [Delete count]
  where
    partial context = take (length (partialLine context) - count) (partialLine context)

-- Helper function to conditionally remove a string if it appears at the end of
-- the output.
slurp :: String -> Env
slurp str = do
  context <- get
  put (Context (lineBreak context) (partial context))
  return [Slurp str]
  where
    -- eg. (partialLine context) | str        | result
    --     ----------------------|------------|-------
    --     ""                    | "\n"       | ""
    --     "foo"                 | "\n"       | "foo"
    --     "foo"                 | "bar"      | "foo"
    --     "abc"                 | "bc"       | "a"
    --     "abc"                 | "foo\nabc" | ""
    --
    -- Note: That last one is unsafe, because we can't guarantee that "foo" is
    -- there. Caveat emptor!
    partial context = if str `isSuffixOf` partialLine context
                      then take (length (partialLine context) - length str) (partialLine context)
                      else partialLine context

defaultLineBreak :: String
defaultLineBreak = "\n"

nodes :: [Node] -> Env
nodes ns = concat <$> mapM node ns

node :: Node -> Env
node n = case n of
  Blockquote b               -> blockquote b >>= nl >>= nl
  BreakTag                   -> breaktag
  Code c                     -> append $ "`" ++ c ++ "`"
  CommandAnnotation {}       -> command n
  CommandsAnnotation         -> heading "commands"
  DocBlock d                 -> nodes d
  Fenced f                   -> fenced f
  FunctionsAnnotation        -> heading "functions"
  FunctionDeclaration {}     -> nodes $ functionBody n
  HeadingAnnotation h        -> heading h
  Link l                     -> append $ link l
  LinkTargets l              -> linkTargets l True
  List ls                    -> nodes ls >>= nl
  ListItem l                 -> listitem l
  MappingAnnotation m        -> mapping m
  MappingsAnnotation         -> heading "mappings"
  OptionAnnotation {}        -> option n
  OptionsAnnotation          -> heading "options"
  Paragraph p                -> nodes p >>= nl >>= nl
  Plaintext p                -> plaintext p
  PluginAnnotation name desc -> plugin name desc
  Project p                  -> nodes p
  Separator                  -> append $ "---" ++ "\n\n"
  SubheadingAnnotation s     -> append $ s ++ " ~\n\n"
  TOC t                      -> toc t
  Unit u                     -> nodes u
  Whitespace                 -> whitespace
  _                          -> append ""

-- TODO: add {name}.txt to the symbol table?
plugin :: String -> String -> Env
plugin name desc = append $
  "*" ++ name ++ ".txt*" ++
  "    " ++ desc ++ "      " ++
  "*" ++ name ++ "*" ++ "\n\n"

-- | Append a newline.
nl :: [Operation] -> Env
nl os = liftM2 (++) (return os) (append "\n")

breaktag :: Env
breaktag = do
  state <- get
  append $ lineBreak state

listitem :: [Node] -> Env
listitem l = do
  context <- get
  -- TODO: consider using lenses to modify records
  put (Context customLineBreak (partialLine context))
  item <- liftM2 (++) (append "- ") (nodes l) >>= nl
  put (Context defaultLineBreak (partialLine context))
  return item
  where
    customLineBreak = "\n  "

toc :: [String] -> Env
toc t = do
  metadata <- ask
  h <- heading "contents"
  entries <- append $ intercalate "\n" (format $ fromJust $ pluginName metadata) ++ "\n\n"
  return $ concat [h, entries]
  where
    -- TODO: fix up this mess (very complicated by the need to thread through
    -- `suffix` everywhere
    format suffix            = map pad (zip (numbered suffix) (repeat suffix))
    longest suffix           = maximum (map (\x -> length (snd x)) (numbered suffix))
    numbered suffix          = map prefix (number suffix)
    number suffix            = zip3 [1..] t (map (\x -> normalize $ x ++ "-" ++ suffix) t)
    prefix (num, desc, l)    = (show num ++ ". " ++ desc ++ "  ", l)
    pad ((lhs, rhs), suffix) = lhs ++ replicate (longest suffix - length lhs) ' ' ++ link rhs
  -- TODO: consider doing this for markdown format too

command :: Node -> Env
command (CommandAnnotation name params) = do
  lhs <- append $ concat [":", name, " ", fromMaybe "" params]
  ws <- append " "
  target <- linkTargets [":" ++ name] False
  trailing <- append "\n"
  return $ concat [lhs, ws, target, trailing]
-- TODO indent what follows until next annotation...
-- will require us to hoist it up inside CommandAnnotation
-- (and do similar for other sections)
-- once that is done, drop the extra newline above

mapping :: String -> Env
mapping name = linkTargets [name] True

option :: Node -> Env
option (OptionAnnotation n t d) = do
  targets <- linkTargets [n] True
  opt <- appendNoWrap $ link n
  ws <- appendNoWrap " "
  context <- get
  meta <- appendNoWrap $ aligned context
  return $ concat [targets, opt, ws, meta]
  where
    aligned context = rightAlign context rhs
    rhs = t ++ " (default: " ++ fromMaybe "none" d ++ ")\n\n"

whitespace :: Env
whitespace = append " "

blockquote :: [Node] -> Env
blockquote ps = do
  context <- get
  put (Context customLineBreak (partialLine context))
  ps' <- mapM paragraph ps
  put (Context defaultLineBreak (partialLine context))
  liftM2 (++) (append "    ") (liftM2 intercalate customParagraphBreak (return ps'))
  where
    -- Strip off trailing newlines from each paragraph.
    paragraph p = fmap trim (node p)
    trim contents = take (length contents - 2) contents
    customLineBreak = "\n    "
    customParagraphBreak = append "\n\n    "

plaintext :: String -> Env
plaintext = append

fenced :: [String] -> Env
fenced f = do
  cut <- slurp "\n"
  prefix <- append ">\n"
  body <- if null f
          then append ""
          else append $ "    " ++ intercalate "\n    " f ++ "\n"
  suffix <- append "<\n"
  return $ concat [cut, prefix, body, suffix]

heading :: String -> Env
heading h = do
  metadata <- ask
  heading' <- appendNoWrap $ map toUpper h ++ " "
  target <- maybe (append "\n") (\x -> linkTargets [target x] False) (pluginName metadata)
  trailing <- append "\n"
  return $ concat [heading', target, trailing]
  where
    target x = normalize $ x ++ "-" ++ h

normalize :: String -> String
normalize = map (toLower . sanitize)

sanitize :: Char -> Char
sanitize x = if isSpace x then '-' else x

link :: String -> String
link l = "|" ++ l ++ "|"

-- TODO: be prepared to wrap these if there are a lot of them
-- TODO: fix code smell of passing in `wrap` bool here
linkTargets :: [String] -> Bool -> Env
linkTargets ls wrap = do
  context <- get
  if wrap
  then append $ aligned context
  else appendNoWrap $ aligned context
  where
    aligned context = rightAlign context (targets ++ "\n")
    targets = unwords (map linkify $ sort ls)
    linkify l = "*" ++ l ++ "*"

rightAlign :: Context -> String -> String
rightAlign context string = align (partialLine context)
  where
    align used = replicate (count used string) ' ' ++ string
    count used xs = maximum [textwidth - renderedWidth xs - renderedWidth used, 0]

-- Crude approximation for calculating rendered width, that does so by not
-- counting the relatively rare |, *, ` and "\n" -- all of which usually get
-- concealed in the rendered output.
renderedWidth :: String -> Int
renderedWidth = foldr reduce 0
  where reduce char acc = if char `elem` "\n|*`"
                        then acc
                        else acc + 1
