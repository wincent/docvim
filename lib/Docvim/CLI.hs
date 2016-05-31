{-# LANGUAGE MultiWayIf #-}

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
import qualified Docvim.Visitor.Footer as Footer (extract)
import qualified Docvim.Visitor.Plugin as Plugin (extract)
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
  let (ast, footer) = Footer.extract $ Project parsed
  let (ast2, plugin) = Plugin.extract ast
  let project = Project $ concat [plugin, [ast2], footer]
  let targets = fromMaybe [""] (outfiles opts)
  mapM_ (\target ->
      -- TODO use MultiWayIf here
      -- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions
      -- and possibly LambdaCase as well
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
