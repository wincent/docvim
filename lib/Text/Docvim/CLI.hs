{-# LANGUAGE MultiWayIf #-}

-- | The runnable part of the docvim executable.
module Text.Docvim.CLI (run) where

import Control.Monad
import Data.Maybe
import System.FilePath hiding (hasExtension)
import System.IO
import Text.Docvim.Compile
import Text.Docvim.Options
import Text.Docvim.Parse
import Text.Docvim.Printer.Markdown
import Text.Docvim.Printer.Vim
import Text.Docvim.ReadDir

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
  let project = compile parsed
  let targets = fromMaybe [""] (outfiles opts)
  mapM_ (\target ->
      if | target == "" -> do
            when (verbose opts) (hPutStrLn stderr "No output target: defaulting to standard out")
            putStrLn $ markdown project
         | isText target -> do
            when (verbose opts) (hPutStrLn stderr ("Outputting in text format to " ++ target))
            writeFile target (vimHelp project)
         | isMarkdown target -> do
            when (verbose opts) (hPutStrLn stderr ("Outputting in markdown format to " ++ target))
            writeFile target (markdown project)
         | otherwise -> hPutStrLn stderr ("Unrecognized output format for " ++ target)
    ) targets
  return ()
