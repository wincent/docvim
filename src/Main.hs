-- | The docvim executable.
module Main (main) where

import Options.Applicative

data Options = Options
  { directory :: String
  , debug :: Bool }

options :: Parser Options
options = Options
     <$> strOption
         ( long "directory"
        <> short 'c'
        <> metavar "DIRECTORY"
        <> help "Change to DIRECTORY before processing" )
     <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug information during processing" )


run :: Options -> IO ()
run (Options d False) = putStrLn "debugging off"
run (Options d True) = putStrLn "debugging on"

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
        (  fullDesc
        <> progDesc "Generate documentation for a Vim plug-in"
        <> header "docim - a documentation generator for Vim plug-ins" )
