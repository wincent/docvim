-- | The docvim executable.
module Main (main) where

import Options (Options(..), options)
import ReadDir (readDir)
import System.FilePath (takeExtension)

run :: Options -> IO ()
run (Options _ _ directory _) = do
  paths <- readDir directory
  let filtered = filter (\path -> takeExtension path == ".vim") paths
  contents <- mapM readFile filtered
  putStrLn $ show contents

-- | Run the executable using the supplied options.
main :: IO ()
main = options >>= run
