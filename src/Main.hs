-- | The docvim executable.
module Main (main) where

-- TODO: for now skip vimscript; later, parse it and use it to enrich the docs
-- or take some manual work out of specifying what's going on
import Control.Applicative
  ( (*>)
  , (<$)
  , (<$>)
  , (<*)
  , (<*>)
  , (<|>)
  , many
  )
-- TODO: custom error messages with <?>
import Options (Options(..), options)
import ReadDir (readDir)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (many1)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Char (anyChar)

data Node = TranslationUnit [Node]
          | VimScript String
          | Comment String
          -- ...everything else
  deriving (Eq, Show)

-- | Parses a translation unit (file contents) into an AST.
parseUnit :: Parser Node
parseUnit = TranslationUnit
  <$> (many parseNode)

parseNode :: Parser Node
parseNode = parseVimScript

parseVimScript :: Parser Node
parseVimScript = VimScript <$> many1 anyChar

-- To test in the REPL:
-- Text.Parsec.runParser parseUnit () "this" "that"
parse :: Parser Node -> String -> IO Node
parse p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure

run :: Options -> IO ()
run (Options _ _ directory _) = do
  paths <- readDir directory
  let filtered = filter (\path -> takeExtension path == ".vim") paths
  parsed <- mapM (parse parseUnit) filtered
  print parsed
  return ()

-- | Run the executable using the supplied options.
main :: IO ()
main = options >>= run
