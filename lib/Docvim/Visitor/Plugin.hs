module Docvim.Visitor.Plugin ( getPluginName
                             , extract
                             ) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad ((>=>))
import Control.Monad.Trans.Writer
import qualified Data.DList as DList
import Data.Data.Lens
import Docvim.AST

-- | Returns the name of the plug-in or Nothing if none is found.
--
-- In the event that there are multiple `@plugin` annotations competing to
-- define the name of plugin, the first encountered one wins.
getPluginName :: Node -> Maybe String
getPluginName node = name
  where
    name = if null names
           then Nothing
           else Just $ head names
    names = walk getName [] node
    getName (PluginAnnotation name _) = [name]
    getName _                         = []

-- | Extracts a list of nodes (if any exist) from the `@plugin` section(s) of
-- the source code.
--
-- It is not recommended to have multiple `@plugin` sections in a project. If
-- multiple such sections (potentially across multiple translation units) exist,
-- there are no guarantees about order; they just get concatenated in the order
-- we see them.
extract :: Node -> (Node, [Node])
extract = toList . runWriter . postorder uniplate extractNodePlugins
  where toList (ast, dlist) = (ast, concat $ DList.toList dlist)

-- | Returns True if a node marks the end of a plugin region.
endPlugin :: Node -> Bool
endPlugin n = case n of
  CommandAnnotation _ -> True
  FooterAnnotation       -> True
  FunctionAnnotation _   -> True
  MappingAnnotation _    -> True
  MappingsAnnotation     -> True
  OptionAnnotation {}    -> True
  PluginAnnotation {}    -> True
  _                      -> False

extractNodePlugins :: Node -> Writer (DList.DList [Node]) Node
extractNodePlugins (DocBlock nodes) = do
    let (plugins, remainder) = extractPlugins nodes
    tell (DList.fromList plugins)
    return (DocBlock remainder)
extractNodePlugins node = return node

extractBlocks :: Alternative f => (a -> Maybe (a -> Bool)) -> [a] -> (f [a], [a])
extractBlocks start = go
  where
    go     [] = (empty, [])
    go (x:xs) = maybe no_extract extract (start x)
      where
        no_extract = (extracted, x:unextracted)
          where
            ~(extracted, unextracted) = go xs
        extract stop = (pure (x:block) <|> extracted, unextracted)
          where
            ~(block, remainder) = break stop xs
            ~(extracted, unextracted) = go remainder
-- TODO factor this out, probably moving it into the AST module

extractPlugins :: Alternative f => [Node] -> (f [Node], [Node])
extractPlugins = extractBlocks f
  where
    f x = case x of
      PluginAnnotation {} -> Just endPlugin
      _                   -> Nothing

postorder :: Monad m => ((a -> m c) -> (a -> m b)) -> (b -> m c) -> (a -> m c)
postorder t f = go
  where
    go = t go >=> f
