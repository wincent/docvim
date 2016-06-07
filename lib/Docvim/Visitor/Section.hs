{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Docvim.Visitor.Section (getSectionInfo) where

import Control.Lens
import Control.Monad.State
import Data.Data.Lens (uniplate)
import Docvim.AST

data SectionInfo = SectionInfo { _hasCommand :: Bool
                               , _hasCommands :: Bool
                               , _hasFunction :: Bool
                               , _hasFunctions :: Bool
                               , _hasMapping :: Bool
                               , _hasMappings :: Bool
                               , _hasOption :: Bool
                               , _hasOptions :: Bool
                               } deriving (Show)

type Env = State SectionInfo

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
getSectionInfo n = execState (mapM check nodes) defaultSectionInfo
  where
    nodes = n ^.. cosmosOf uniplate
    check (CommandAnnotation {}) = hasCommand .= True
    check CommandsAnnotation     = hasCommands .= True
    check (FunctionAnnotation _) = hasFunction .= True
    check FunctionsAnnotation    = hasFunctions .= True
    check (MappingAnnotation _)  = hasMapping .= True
    check MappingsAnnotation     = hasMappings .= True
    check (OptionAnnotation {})  = hasOption .= True
    check OptionsAnnotation      = hasOptions .= True
    check _                      = do
      state <- get
      put state
