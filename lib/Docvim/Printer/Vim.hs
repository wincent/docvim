module Docvim.Printer.Vim
  ( vimHelp
  , ppv
  , pv
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toUpper)
import Data.List (intercalate, sort)
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
               | Delete String -- note that String may not be the right thing
data Metadata = Metadata { symbols :: [String]
                         , pluginName :: Maybe String
                         }
data Context = Context { lineBreak :: String
                       , partialLine :: String
                       }
type Env = ReaderT Metadata (State Context) [Operation]

vimHelp :: Node -> String
vimHelp n = rstrip output ++ "\n"
  where metadata = Metadata (getSymbols n) (getPluginName n)
        context = Context defaultLineBreak ""
        operations = evalState (runReaderT (node n) metadata) context
        output = foldl reduce "" operations
        -- TODO: handle rollbacks as well
        -- probably need some tuple fanciness for the accumulator
        -- (accumulatedOutput, lastAtom)
        reduce acc (Append atom) = acc ++ atom

-- Helper function that appends and updates `partialLine` context.
append :: String -> Env
append string = do
  context <- get
  put (Context (lineBreak context) (partial context))
  return [Append string]
  where
    partial context = if length end == length string
                      then partialLine context ++ end
                      else end
    end = reverse $ takeWhile (/= '\n') (reverse string)

defaultLineBreak :: String
defaultLineBreak = "\n"

nodes :: [Node] -> Env
nodes ns = concat <$> mapM node ns

-- TODO: deal with hard-wrapping
node :: Node -> Env
node n = case n of
  -- Nodes that depend on (or must propagate) reader/state context.
  Blockquote b            -> blockquote b >>= nl >>= nl
  BreakTag                -> breaktag
  DocBlock d              -> nodes d
  FunctionDeclaration {}  -> nodes $ functionBody n
  Paragraph p             -> nodes p >>= nl >>= nl
  Plaintext p             -> plaintext p
  Link l                  -> link l
  List ls                 -> nodes ls >>= nl
  ListItem l              -> listitem l
  Project p               -> nodes p
  Unit u                  -> nodes u
  -- TODO deal with hard wrapping: here might be a good place for it...
  Whitespace              -> whitespace

  -- Nodes that don't depend on reader context.
  Code c                  -> append $ "`" ++ c ++ "`"
  Fenced f                -> return $ fenced f
  -- TODO: Vim will only highlight this as a heading if it has a trailing
  -- LinkTarget on the same line; figure out how to handle that; may need to
  -- address it in the Parser
  --
  -- Looking at the Ferret fixtures, seems like I had an idea for this which was
  -- to auto-gen the targets based on the plugin name + the heading text.
  --
  -- I could also just make people specify a target explicitly.
  HeadingAnnotation h     -> append $ map toUpper h ++ "\n\n"
  LinkTargets l           -> return $ linkTargets l : [Append "\n"]
  -- TODO: this should be order-independent and always appear at the top.
  -- Note that I don't really have anywhere to put the description; maybe I should
  -- scrap it (nope: need it in the Vim help version).
  PluginAnnotation name desc -> plugin name desc
  Separator               -> append $ "---" ++ "\n\n"
  SubheadingAnnotation s  -> append $ s ++ " ~\n\n"
  _                       -> append ""

-- TODO: right-align trailing link target
-- TODO: add {name}.txt to the symbol table?
plugin :: String -> String -> Env
plugin name desc = append $
  "*" ++ name ++ ".txt*" ++
  "    " ++ desc ++ "      " ++
  "*" ++ name ++ "*" ++ "\n\n"

-- | Append a newline.
nl :: [Operation] -> Env
nl os = return $ os ++ [Append "\n"]

breaktag :: Env
breaktag = do
  state <- get
  append $ lineBreak state

listitem :: [Node] -> Env
listitem l = do
  context <- get
  -- TODO: consider using lenses to modify records
  put (Context customLineBreak (partialLine context))
  item <- fmap ([Append "- "] ++) (nodes l) >>= nl
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
  return $ Append "    " : intercalate [customParagraphBreak] ps'
  where
    -- Strip off trailing newlines from each paragraph.
    paragraph p = fmap trim (node p)
    trim contents = take (length contents - 2) contents
    customLineBreak = "\n    "
    customParagraphBreak = Append "\n\n    "

-- TODO: based on current line length, decide whether to override
-- linebreak or not
-- problem is that we will already have emitted a whitespace by the time we
-- get here (which means we will wind up with trailing whitespace in the
-- output...)
-- and if we instead handle it in the whitespace function, we have no way of
-- looking ahead to see the length of the plaintext
--
-- ways to deal with this... figure out some kind of rollback
-- implement pending whitespace and check it everywhere we print something...
-- or: instead of appending to a string, append a list of operations eg
-- [append "foo"], [append " "], [delete " "] etc...
-- or: post process to strip all trailing whitespace (probably the easiest
-- thing)
plaintext :: String -> Env
plaintext = append

-- TODO: handle "interesting" link text like containing [, ], "
link :: String -> Env
link l = do
  metadata <- ask
  if l `elem` symbols metadata
  then append $ "|" ++ l ++ "|"
  -- TODO: figure out what to do here
  -- probably want to treat URLs specially
  -- and Vim help links, obviously
  else append l

-- TODO ideally want to replace preceding blank line with >, not append one
-- and likewise, replace following blank line with <
-- (but this will be tricky; could do it as a post-processing step)
fenced :: [String] -> [Operation]
fenced f = [Append ">\n"] ++ code ++ [Append "<\n"]
  where code = if null f
               then [Append ""]
               else Append "    " : [Append $ intercalate "\n    " f ++ "\n"]

-- TODO: be prepared to wrap these if there are a lot of them
linkTargets :: [String] -> Operation
linkTargets ls = Append $ rightAlign targets
  where
    targets = unwords (map linkify $ sort ls)
    linkify l = "*" ++ l ++ "*"
    rightAlign ws = replicate (count ws) ' ' ++ ws
    count xs = maximum [78 - length xs, 0]

-- | For unit testing.
pv :: String -> String
pv input = case parseUnit input of
            Left error -> show error
            Right ast -> vimHelp ast

-- | For logging in the REPL.
ppv :: String -> IO ()
ppv = putStr . pv
