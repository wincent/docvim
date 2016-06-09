{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Docvim.Visitor.Section ( injectCommands
                                   , injectFunctions
                                   , injectMappings
                                   , injectOptions
                                   ) where

import Control.Lens
import Control.Monad.State
import Data.Data.Lens (uniplate)
import Text.Docvim.AST

data SectionInfo = SectionInfo { _hasCommand :: Bool
                               , _hasCommands :: Bool
                               , _hasFunction :: Bool
                               , _hasFunctions :: Bool
                               , _hasMapping :: Bool
                               , _hasMappings :: Bool
                               , _hasOption :: Bool
                               , _hasOptions :: Bool
                               } deriving (Show)

-- Could also have written record setters by hand, but too lazy to do this:
--
--     setHasCommand :: SectionInfo -> SectionInfo
--     setHasCommand info = info { hasCommand = True }
--
-- With lenses, we can auto-generate functions that we call like this:
--
--     view hasCommand info             (reading)
--     info ^. hasCommand               (reading, using operator)
--     set hasCommand True info         (writing)
--     info & hasCommand .~ True        (writing, using operators)
--
-- Or, given that we are using the State monad here, we'll be using the `.=`
-- operator to update the state using a lens.
--
makeLenses ''SectionInfo

defaultSectionInfo :: SectionInfo
defaultSectionInfo = SectionInfo { _hasCommand = False
                                 , _hasCommands = False
                                 , _hasFunction = False
                                 , _hasFunctions = False
                                 , _hasMapping = False
                                 , _hasMappings = False
                                 , _hasOption = False
                                 , _hasOptions = False
                                 }

-- | Walks the supplied AST detecting whether it contains
-- `@commands`/`@command`, `@functions`/`@function`, `@mappings`/`@mapping` or
-- `@options`/`@options` sections.
--
-- Will be used as follows:
--   - DO have @commands? -> do nothing
--   - DON'T have @commands but DO have @command? -> Synthesize CommandsAnnotation
--   - DON'T we have either? -> do nothing
--
getSectionInfo :: Node -> SectionInfo
getSectionInfo n = execState (mapMOf_ (cosmosOf uniplate) check n) defaultSectionInfo
  where
    check CommandAnnotation {}   = hasCommand .= True
    check CommandsAnnotation     = hasCommands .= True
    check (FunctionAnnotation _) = hasFunction .= True
    check FunctionsAnnotation    = hasFunctions .= True
    check (MappingAnnotation _)  = hasMapping .= True
    check MappingsAnnotation     = hasMappings .= True
    check OptionAnnotation {}    = hasOption .= True
    check OptionsAnnotation      = hasOptions .= True
    check _                      = modify id

-- | Appends a node to the end of a Project.
inject :: Node -> Node -> Node
inject (Project ns) n = Project $ ns ++ [n]
inject other _ = other

injectCommands :: Node -> Node
injectCommands n =
  if | getSectionInfo n ^. hasCommands -> n
     | getSectionInfo n ^. hasCommand -> inject n CommandsAnnotation
     | otherwise -> n

injectFunctions :: Node -> Node
injectFunctions n =
  if | getSectionInfo n ^. hasFunctions -> n
     | getSectionInfo n ^. hasFunction -> inject n FunctionsAnnotation
     | otherwise -> n

injectMappings :: Node -> Node
injectMappings n =
  if | getSectionInfo n ^. hasMappings -> n
     | getSectionInfo n ^. hasMapping -> inject n MappingsAnnotation
     | otherwise -> n

injectOptions :: Node -> Node
injectOptions n =
  if | getSectionInfo n ^. hasOptions -> n
     | getSectionInfo n ^. hasOption -> inject n OptionsAnnotation
     | otherwise -> n
