-- | The runnable part of the docvim executable.
module Docvim.CLI (run) where

import Control.Monad (when)
import Docvim.Options (Options(..), options)
import Docvim.Parse (parse)
import Docvim.ReadDir (readDir)
import System.FilePath (takeExtension)

isVimScript :: FilePath -> Bool
isVimScript = (==) ".vim" . takeExtension

run :: IO ()
run = do
  opts <- options
  paths <- readDir (directory opts)
  let filtered = filter isVimScript paths
  parsed <- mapM (\path -> do
      when (verbose opts) (putStrLn $ "Processing " ++ path)
      parse path
    ) filtered
  print parsed
  return ()
