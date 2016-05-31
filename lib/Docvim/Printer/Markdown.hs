module Docvim.Printer.Markdown
  ( markdown
  , pm
  , ppm
  ) where

import Control.Monad.Reader
import Data.List (intercalate, sort)
import Docvim.AST
import Docvim.Parse (parseUnit, strip)
import Docvim.Visitor.Plugin (getPluginName)
import Docvim.Visitor.Symbol (getSymbols)

data Metadata = Metadata { symbols :: [String]
                         , pluginName :: Maybe String
                         }
type Env = Reader Metadata String

data Anchor = Anchor [Attribute] String
data Attribute = Attribute { attributeName :: String
                           , attributeValue :: String
                           }

markdown :: Node -> String
markdown n = strip (runReader (node n) state) ++ "\n"
  where state = Metadata (getSymbols n) (getPluginName n)

nodes :: [Node] -> Env
nodes ns = concat <$> mapM node ns

node :: Node -> Env
node n = case n of
  -- Nodes that depend on (or must propagate) reader context.
  Blockquote b            -> blockquote b >>= nl >>= nl
  DocBlock d              -> nodes d
  FunctionDeclaration {}  -> nodes $ functionBody n
  Paragraph p             -> nodes p >>= nl >>= nl
  Link l                  -> link l
  List ls                 -> nodes ls >>= nl
  ListItem l              -> fmap ("- " ++) (nodes l) >>= nl
  Project p               -> nodes p
  Unit u                  -> nodes u

  -- Nodes that don't depend on reader context.
  BreakTag                -> return "<br />"
  Code c                  -> return $ "`" ++ c ++ "`"
  Fenced f                -> return $ fenced f ++ "\n\n"
  HeadingAnnotation h     -> return $ "## " ++ h ++ "\n\n"
  LinkTargets l           -> return $ linkTargets l ++ "\n"
  -- TODO: this should be order-independent and always appear at the top.
  -- Note that I don't really have anywhere to put the description; maybe I should
  -- scrap it (nope: need it in the Vim help version).
  PluginAnnotation name _ -> return $ "# " ++ name ++ "\n\n"
  Plaintext p             -> return p
  Separator               -> return $ "---" ++ "\n\n"
  SubheadingAnnotation s  -> return $ "### " ++ s ++ "\n\n"
  Whitespace              -> return " "
  _                       -> return ""

-- | Append a newline.
nl :: String -> Env
nl = return . (++ "\n")

blockquote :: [Node] -> Env
blockquote ps = do
  ps' <- mapM paragraph ps
  return $ "> " ++ intercalate "\n>\n> " ps'
  where
    -- Strip off trailing newlines from each paragraph.
    paragraph p = fmap trim (node p)
    trim contents = take (length contents - 2) contents

-- TODO: handle "interesting" link text like containing [, ], "
link :: String -> Env
link l = do
  state <- ask
  return $ if l `elem` symbols state
           -- TODO: beware names with < ` etc in them
           then "<strong>[`" ++ l ++ "`](" ++ gitHubAnchor l ++ ")</strong>"
           else "<strong>`" ++ l ++ "`</strong>" -- TODO:
                                -- may want to try producing a link to Vim
                                -- online help if I can find a search for it

fenced :: [String] -> String
fenced f = "```\n" ++ code ++ "```"
  where code = if null f
               then ""
               else intercalate "\n" f ++ "\n"

linkTargets :: [String] -> String
linkTargets ls =  "<p align=\"right\">"
               ++ unwords (map linkify $ sort ls)
               ++ "</p>"
  where linkify l = a $ Anchor [ Attribute "name" (sanitizeAnchor l)
                               , Attribute "href" (gitHubAnchor l)
                               ]
                               ("<code>" ++ l ++ "</code>")

a :: Anchor -> String
a (Anchor attributes target) = "<a" ++ attrs ++ ">" ++ target ++ "</a>"
  where
    attrs = if not (null attributes)
            then " " ++ attributesString attributes
            else ""

attributesString :: [Attribute] -> String
attributesString as = unwords (map attributeToString as)
  where attributeToString (Attribute name value) = name ++ "=\"" ++ value ++ "\""

gitHubAnchor :: String -> String
gitHubAnchor n = "#user-content-" ++ sanitizeAnchor n

-- | For unit testing.
pm :: String -> String
pm input = case parseUnit input of
            Left error -> show error
            Right ast -> markdown ast

-- | For logging in the REPL.
ppm :: String -> IO ()
ppm = putStr . pm
