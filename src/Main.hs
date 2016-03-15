-- | The docvim executable.
module Main (main) where

import Options.Applicative

data Config = Config
  { directory :: String
  , debug :: Bool }

config :: Parser Config
config = Config
     <$> strOption
         ( long "directory"
        <> short 'c'
        <> metavar "DIRECTORY"
        <> help "Change to DIRECTORY before processing" )
     <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug information during processing" )


intro :: Config -> IO ()
intro (Config d False) = putStrLn "debugging off"
intro (Config d True) = putStrLn "debugging on"

main :: IO ()
main = execParser opts >>= intro
  where
    opts = info (helper <*> config)
        (  fullDesc
        <> progDesc "Generate documentation for a Vim plug-in"
        <> header "docim - a documentation generator for Vim plug-ins" )
