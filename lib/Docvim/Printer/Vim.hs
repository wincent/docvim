module Docvim.Printer.Vim
  ( vimHelp
  , ppv
  , pv
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isSpace, toUpper)
import Data.List (intercalate, isSuffixOf, sort)
import Docvim.AST
import Docvim.Parse (parseUnit, rstrip)
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
vimHelp n = rstrip output ++ "\n"
  where metadata = Metadata (getSymbols n) (getPluginName n)
        context = Context defaultLineBreak ""
        operations = evalState (runReaderT (node n) metadata) context
        output = foldl reduce "" operations
        reduce acc (Append atom) = acc ++ atom
        reduce acc (Delete count) = take (length acc - count) acc
        reduce acc (Slurp atom) = if isSuffixOf atom acc
                                  then take (length acc - length atom) acc
                                  else acc

-- Helper function that appends and updates `partialLine` context.
append :: String -> Env
append string = do
  context <- get
  -- TODO make that >=
  -- TODO obviously tidy this up
  -- TODO instead of deleting trailing whitespace (might not actually be any)
  -- delete back to whitespace, then replay non-whitespace bits; should fix bad
  -- output like this:
  --     Searches for {pattern} in all the files under the current directory (see :pwd
  --     ), unless otherwise overridden via {options}, and displays the results in the
  -- TODO: always suppress trailing whitespace (some of it is making it into the
  -- output)
  let (ops, line) = if length (partialLine context) + length string >= textwidth
                    then ([Delete (trailing $ partialLine context), Append (lineBreak context), Append $ slurpWhitespace string], lineBreak context ++ slurpWhitespace string)
                    else ([Append string], partialLine context ++ string)
  put (Context (lineBreak context) (end line))
  return ops
  where
    trailing str = length $ takeWhile isSpace (reverse str)
    end l = reverse $ takeWhile (/= '\n') (reverse l)
    slurpWhitespace atom = if atom == " "
                           then ""
                           else atom

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
    partial context = if isSuffixOf str (partialLine context)
                      then take (length (partialLine context) - length str) (partialLine context)
                      else (partialLine context)

defaultLineBreak :: String
defaultLineBreak = "\n"

nodes :: [Node] -> Env
nodes ns = concat <$> mapM node ns

-- TODO: deal with hard-wrapping (still some overlength lines and edge cases to
-- deal with)
node :: Node -> Env
node n = case n of
  Blockquote b               -> blockquote b >>= nl >>= nl
  BreakTag                   -> breaktag
  Code c                     -> append $ "`" ++ c ++ "`"
  DocBlock d                 -> nodes d
  Fenced f                   -> fenced f
  FunctionDeclaration {}     -> nodes $ functionBody n
  -- TODO: Vim will only highlight this as a heading if it has a trailing
  -- LinkTarget on the same line; figure out how to handle that; may need to
  -- address it in the Parser
  --
  -- Looking at the Ferret fixtures, seems like I had an idea for this which was
  -- to auto-gen the targets based on the plugin name + the heading text.
  --
  -- I could also just make people specify a target explicitly.
  HeadingAnnotation h        -> append $ map toUpper h ++ "\n\n"
  Link l                     -> append $ "|" ++ l ++ "|"
  LinkTargets l              -> linkTargets l
  List ls                    -> nodes ls >>= nl
  ListItem l                 -> listitem l
  Paragraph p                -> nodes p >>= nl >>= nl
  Plaintext p                -> plaintext p
  -- TODO: this should be order-independent and always appear at the top.
  -- Note that I don't really have anywhere to put the description; maybe I should
  -- scrap it (nope: need it in the Vim help version).
  PluginAnnotation name desc -> plugin name desc
  Project p                  -> nodes p
  Separator                  -> append $ "---" ++ "\n\n"
  SubheadingAnnotation s     -> append $ s ++ " ~\n\n"
  Unit u                     -> nodes u
  Whitespace                 -> whitespace
  _                          -> append ""

-- TODO: right-align trailing link target
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

whitespace :: Env
whitespace =
  -- if current line > 80 "\n" else " "
  -- but note, really need to do this BEFORE 80
  append " "

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

-- TODO: be prepared to wrap these if there are a lot of them
linkTargets :: [String] -> Env
linkTargets ls = append $ rightAlign targets ++ "\n"
  where
    targets = unwords (map linkify $ sort ls)
    linkify l = "*" ++ l ++ "*"
    rightAlign ws = replicate (count ws) ' ' ++ ws
    count xs = maximum [textwidth - length xs, 0]

-- | For unit testing.
pv :: String -> String
pv input = case parseUnit input of
            Left error -> show error
            Right ast -> vimHelp ast

-- | For logging in the REPL.
ppv :: String -> IO ()
ppv = putStr . pv
