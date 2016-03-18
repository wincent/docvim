-- | The docvim executable.
module Main (main) where

import Options (Options(..), options)
import Parse (parse)
import ReadDir (readDir)
import System.FilePath (takeExtension)

run :: Options -> IO ()
run (Options _ _ directory verbose) = do
  paths <- readDir directory
  let filtered = filter (\path -> takeExtension path == ".vim") paths
  parsed <- mapM (\path -> do
      if verbose then putStrLn $ "Processing " ++ path
      else return ()
      parse path
    ) filtered
  print parsed
  return ()

-- | Run the executable using the supplied options.
main :: IO ()
main = options >>= run
