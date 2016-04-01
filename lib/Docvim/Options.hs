-- | Options parser for the docvim executable.
module Docvim.Options (Options(..), options) where

import Options.Applicative
import Data.Version (showVersion)
import qualified Paths_docvim (version)

-- TODO: figure out where (and if!) to expand ~ and such in path options
data Options = Options
  { outfiles :: Maybe [FilePath]
  , debug :: Bool
  , directory :: String
  , verbose :: Bool }

parseOutfiles :: Parser [String]
parseOutfiles = some $ argument str
  (  metavar "OUTFILES..."
  <> help (unlines [ "Target file(s) for generated output"
                   , "(default: standard output)" ]))

version :: Parser (a -> a)
version = infoOption (showVersion Paths_docvim.version)
  (  long "version"
  <> help "Print version information" )

parseDebug :: Parser Bool
parseDebug = switch
  $  long "debug"
  <> short 'd'
  <> help "Print debug information during processing"

parseDirectory :: Parser String
parseDirectory = strOption
  $  long "directory"
  <> short 'c'
  <> metavar "DIRECTORY"
  <> value "."
  <> showDefault
  <> help "Change to DIRECTORY before processing"

parseVerbose :: Parser Bool
parseVerbose = switch
  $  long "verbose"
  <> short 'v'
  <> help "Be verbose during processing"

parseOptions :: Parser Options
parseOptions = Options
  <$> optional parseOutfiles
  <*> parseDebug
  <*> parseDirectory
  <*> parseVerbose

options :: IO Options
options = execParser $ info (helper <*> version <*> parseOptions)
  (  fullDesc
  <> progDesc "Generate documentation for a Vim plug-in"
  <> header "docvim - a documentation generator for Vim plug-ins" )
