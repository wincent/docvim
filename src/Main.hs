-- | The docvim executable.
module Main (main) where

import Options (Options(..), options)
import ReadDir (readDir)
import System.FilePath (takeExtension)

run :: Options -> IO ()
run (Options _ _ directory _) = do
  contents <- readDir directory
  let filtered = filter (\path -> takeExtension path == ".vim") contents
  putStrLn $ show filtered

main :: IO ()
-- | Run the executable using the supplied options.
main = options >>= run
