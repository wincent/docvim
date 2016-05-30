-- | The runnable part of the docvim executable.
module Docvim.CLI (run) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Docvim.Options (Options(..), options)
import Docvim.AST (Node(Project))
import Docvim.Parse (parse)
import Docvim.Printer.Markdown (markdown)
import Docvim.Printer.Vim (vimHelp)
import Docvim.ReadDir (readDir)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)

hasExtension :: String -> FilePath -> Bool
hasExtension ext fp = takeExtension fp == ext

isMarkdown :: FilePath -> Bool
isMarkdown = hasExtension ".md"

isText :: FilePath -> Bool
isText = hasExtension ".txt"

isVimScript :: FilePath -> Bool
isVimScript = hasExtension ".vim"

run :: IO ()
run = do
  opts <- options
  paths <- readDir (directory opts)
  let filtered = filter isVimScript paths
  parsed <- mapM (\path -> do
      when (verbose opts) (hPutStrLn stderr ("Parsing " ++ path))
      parse path
    ) filtered
  let project = Project parsed
  let targets = fromMaybe [""] (outfiles opts)
  mapM_ (\target ->
      if target == ""
        then do
          when (verbose opts) (hPutStrLn stderr "No output target: defaulting to standard out")
          putStrLn $ markdown project
        else if isText target
          then do
            when (verbose opts) (hPutStrLn stderr ("Outputting in text format to " ++ target))
            putStrLn "text output [not yet implemented]"
          else if isMarkdown target
            then do
              when (verbose opts) (hPutStrLn stderr ("Outputting in markdown format to " ++ target))
              putStrLn $ markdown project
            else
              hPutStrLn stderr ("Unrecognized output format for " ++ target)
    ) targets
  return ()
