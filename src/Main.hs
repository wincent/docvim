-- | The docvim executable.
module Main (main) where

import Control.Monad (when)
import Options (Options(..), options)
import Parse (parse)
import ReadDir (readDir)
import System.FilePath (takeExtension)

run :: Options -> IO ()
run (Options _ _ directory verbose) = do
  paths <- readDir directory
  let filtered = filter (\path -> takeExtension path == ".vim") paths
  parsed <- mapM (\path -> do
      when verbose (putStrLn $ "Processing " ++ path)
      parse path
    ) filtered
  print parsed
  return ()

-- | Run the executable using the supplied options.
main :: IO ()
main = options >>= run
