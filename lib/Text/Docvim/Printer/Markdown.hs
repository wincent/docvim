module Text.Docvim.Printer.Markdown (markdown) where

import Control.Monad.Reader
import Data.List
import Data.Maybe
import Text.Docvim.AST
import Text.Docvim.Parse
import Text.Docvim.Visitor.Plugin
import Text.Docvim.Visitor.Symbol

data Metadata = Metadata { pluginName :: Maybe String
                         , symbols :: [String]
                         }
type Env = Reader Metadata String

data Anchor = Anchor [Attribute] String
data Attribute = Attribute String String

markdown :: Node -> String
markdown n = if null stripped
             then ""
             else stripped ++ "\n"
  where
    metadata = Metadata (getPluginName n) (getSymbols n)
    stripped = rstrip (runReader (node n) metadata)

nodes :: [Node] -> Env
nodes ns = concat <$> mapM node ns

node :: Node -> Env
node n = case n of
  Blockquote b            -> blockquote b >>= nl >>= nl
  -- TODO, for readability, this should be "<br />\n" (custom, context-aware separator; see Vim.hs)
  BreakTag                -> return "<br />"
  Code c                  -> return $ "`" ++ c ++ "`"
  CommandAnnotation {}    -> command n
  CommandsAnnotation      -> h2 "Commands"
  DocBlock d              -> nodes d
  Fenced f                -> return $ fenced f ++ "\n\n"
  FunctionDeclaration {}  -> nodes $ functionBody n
  FunctionsAnnotation     -> h2 "Functions"
  HeadingAnnotation h     -> h2 h
  Link l                  -> link l
  LinkTargets l           -> return $ linkTargets l
  List ls                 -> nodes ls >>= nl
  ListItem l              -> fmap ("- " ++) (nodes l) >>= nl
  MappingAnnotation m     -> mapping m
  MappingsAnnotation      -> h2 "Mappings"
  OptionAnnotation {}     -> option n
  OptionsAnnotation       -> h2 "Options"
  Paragraph p             -> nodes p >>= nl >>= nl
  Plaintext p             -> return p
  -- TODO: this should be order-independent and always appear at the top.
  -- Note that I don't really have anywhere to put the description; maybe I should
  -- scrap it (nope: need it in the Vim help version).
  PluginAnnotation name _ -> h1 name
  Project p               -> nodes p
  Separator               -> return $ "---" ++ "\n\n"
  SubheadingAnnotation s  -> h3 s
  Unit u                  -> nodes u
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
  metadata <- ask
  return $ if l `elem` symbols metadata
           -- TODO: beware names with < ` etc in them
           -- TODO: consider not using <strong>
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
               ++ "\n"
  where
    linkify l = a $ Anchor [ Attribute "name" (sanitizeAnchor l)
                           , Attribute "href" (gitHubAnchor l)
                           ]
                           (codify l)

h1 :: String -> Env
h1 = heading 1

h2 :: String -> Env
h2 = heading 2

h3 :: String -> Env
h3 = heading 3

heading :: Int -> String -> Env
heading level string = do
  metadata <- ask
  return $ replicate level '#' ++ " " ++ string ++ anch (pluginName metadata) ++ "\n\n"
  where
    anch name = a $ Anchor [ Attribute "name" (sanitizeAnchor $ pre ++ string)
                           , Attribute "href" (gitHubAnchor $ pre ++ string)
                           ]
                           ""
      where
        pre = maybe "" (++ "-") name

-- | Wraps a string in `<code>`/`</code>` tags.
-- TODO: remember why I'm not using backticks here.
codify :: String -> String
codify s = "<code>" ++ s ++ "</code>"

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

-- TODO: make sure symbol table knows about option targets too
option :: Node -> Env
option (OptionAnnotation n t d) = do
  h <- h3 $ "`" ++ n ++ "` (" ++ t ++ ", default: " ++ def ++ ")"
  return $ targets ++ h
  where targets = linkTargets [n]
        def = fromMaybe "none" d
option _ = invalidNode

command :: Node -> Env
command (CommandAnnotation name params) = do
  content <- h3 $ "`:" ++ annotation ++ "`"
  return $ target ++ content
  where target = linkTargets [":" ++ name]
        annotation = rstrip $ name ++ " " ++ fromMaybe "" params
command _ = invalidNode

mapping :: String -> Env
mapping name = h3 $ "`" ++ name ++ "`"
