-- | The docvim executable.
module Main (main) where

import Options (Options(..), options)
import ReadDir (readDir)
import System.FilePath (takeExtension)

run :: Options -> IO ()
run _ = do
  contents <- readDir "."
  let filtered = filter (\path -> takeExtension path == ".vim") contents
  putStrLn $ show filtered
run (Options (Just a) _ _ _) = putStrLn ("got file " ++ a)
run (Options _ False d _) = putStrLn "debugging off"
run (Options _ True d _) = putStrLn "debugging on"

main :: IO ()
-- | Run the executable using the supplied options.
main = options >>= run
