-- | The docvim executable.
module Main (main) where

import Options.Applicative
import Data.Version (showVersion)

import qualified Paths_docvim (version)

data Options = Options
  { outfile :: Maybe FilePath
  , debug :: Bool
  , directory :: String
  , verbose :: Bool }

parseOutfile = argument str
  (  metavar "OUTFILE"
  <> help (unlines [ "Target file for generated output"
                   , "(default: standard output)" ]))

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
  <> help "Change to DIRECTORY before processing"

parseVerbose :: Parser Bool
parseVerbose = switch
  $  long "verbose"
  <> short 'v'
  <> help "Be verbose during processing"

parseOptions :: Parser Options
parseOptions = Options
  <$> optional parseOutfile
  <*> parseDebug
  <*> parseDirectory
  <*> parseVerbose

run :: Options -> IO ()
run (Options (Just a) _ _ _) = putStrLn ("got file " ++ a)
run (Options _ False d _) = putStrLn "debugging off"
run (Options _ True d _) = putStrLn "debugging on"

main :: IO ()
main = execParser options >>= run
  where
    options = info (helper <*> version <*> parseOptions)
            ( fullDesc
            <> progDesc "Generate documentation for a Vim plug-in"
            <> header "docvim - a documentation generator for Vim plug-ins" )
