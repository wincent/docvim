-- | The docvim executable.
module Main (main) where

import Options.Applicative
import Data.Version (showVersion)

import qualified Paths_docvim (version)

data Options = Options
  { debug :: Bool
  , directory :: String }

version = infoOption (showVersion Paths_docvim.version)
  ( long "version"
  <> help "Print version information" )

parseDebug :: Parser Bool
parseDebug = switch
  $ long "debug"
  <> short 'd'
  <> help "Print debug information during processing"

parseDirectory :: Parser String
parseDirectory = strOption
  $ long "directory"
  <> short 'c'
  <> metavar "DIRECTORY"
  <> value "."
  <> help "Change to DIRECTORY before processing"

parseOptions :: Parser Options
parseOptions = Options
  <$> parseDebug
  <*> parseDirectory

run :: Options -> IO ()
run (Options False d) = putStrLn "debugging off"
run (Options True d) = putStrLn "debugging on"

main :: IO ()
main = execParser options >>= run
  where
    options = info (helper <*> version <*> parseOptions)
            ( fullDesc
            <> progDesc "Generate documentation for a Vim plug-in"
            <> header "docvim - a documentation generator for Vim plug-ins" )
