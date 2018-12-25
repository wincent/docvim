module Text.Docvim.Visitor.Symbol (getSymbols) where

import Data.List
import Text.Docvim.AST
import Text.Docvim.Visitor.Plugin
import qualified Data.Set as Set

-- TODO: return Set instead of [String]
-- TODO: use Either instead of dying unceremoniously with `error`
getSymbols :: Node -> [String]
getSymbols node = if length symbols == Set.size set
                  then symbols
                  -- BUG: validation doesn't seem to run at all for:
                  --   ppm "\"\" @plugin ZZZZ thing\n\" # thing\n\" # thingz\n\" # thingy\n\" thing\n\"\n\" # thingz\n\" # thingz\n"
                  -- but it does run for:
                  --   ppm "\"\" @plugin ZZZZ thing\n\" # thing\n\" # thingz\n\" # thingy\n\" |thing|\n\"\n\" # thingz\n\" # thingz\n"
                  -- yet the AST for the first tree does trip us up in here:
                  --   let x (Right y) = y
                  --   let y = parseUnit "\"\" @plugin ZZZZ thing\n\" # thing\n\" # thingz\n\" # thingy\n\" thing\n\"\n\" # thingz\n\" # thingz\n"
                  --   getSymbols (x y) -- BOOM!
                  -- what's making the expection get swallowed in the ppm case?
                  else error $ "Duplicate symbol table entries: " ++ show duplicates
  where
    set                                     = Set.fromList symbols
    symbols                                 = walk gatherSymbols [] node
    gatherSymbols (CommandAnnotation n _)   = [":" ++ n]
    gatherSymbols CommandsAnnotation        = genHeading "commands"
    gatherSymbols FunctionsAnnotation       = genHeading "functions"
    gatherSymbols (HeadingAnnotation h)     = genHeading h
    gatherSymbols (LinkTargets ts)          = ts
    -- TODO: probably don't want this target to exist in the symbol table when
    -- emitting Markdown
    gatherSymbols (PluginAnnotation name _) = [name, name ++ ".txt"]
    gatherSymbols (MappingAnnotation m)     = [m]
    gatherSymbols MappingsAnnotation        = genHeading "mappings"
    gatherSymbols (OptionAnnotation o _ _)  = [o]
    gatherSymbols OptionsAnnotation         = genHeading "options"
    gatherSymbols _                         = []
    genHeading h                            = maybe [] (\x -> [sanitizeAnchor $ x ++ "-" ++ h]) (getPluginName node)
    duplicates                              = nub $ f (sort symbols)
      where
        f [] = []
        f [_] = []
        f (x:xs) = if x == head xs
                   then x : f xs
                   else f xs
