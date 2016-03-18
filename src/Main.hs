-- | The docvim executable.
module Main (main) where

import Control.Monad (when)
import Options (Options(..), options)
import Parse (parse)
import ReadDir (readDir)
import System.FilePath (takeExtension)

isVimScript :: FilePath -> Bool
isVimScript = (==) ".vim" . takeExtension

run :: Options -> IO ()
run (Options _ _ directory verbose) = do
  paths <- readDir directory
  let filtered = filter isVimScript paths
  parsed <- mapM (\path -> do
      when verbose (putStrLn $ "Processing " ++ path)
      parse path
    ) filtered
  print parsed
  return ()

-- | Run the executable using the supplied options.
main :: IO ()
main = options >>= run
