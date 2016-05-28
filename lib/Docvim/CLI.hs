-- | The runnable part of the docvim executable.
module Docvim.CLI (run) where

import Control.Monad (when)
import Docvim.Options (Options(..), options)
import Docvim.AST (Node(Project))
import Docvim.Parse (parse)
import Docvim.Printer.Markdown (markdown)
import Docvim.Printer.Vim (vimHelp)
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
      when (verbose opts) (putStrLn $ "Parsing " ++ path)
      parse path
    ) filtered
  let project = Project parsed
  let md = markdown project
  putStrLn md
  return ()
