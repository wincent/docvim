module Main (main) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Char (chr)
import Data.List (isPrefixOf, sort)
import Data.Monoid (Sum(..))
import Docvim.AST
import Docvim.Util (compileUnit, p, pm, pv)
import Docvim.Visitor.Symbol (getSymbols)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>), replaceExtension, takeBaseName, takeFileName)
import System.IO (hFlush, readFile)
import System.IO.Temp (withSystemTempFile)
import System.Process ( StdStream(CreatePipe)
                      , createProcess
                      , proc
                      , std_out
                      , waitForProcess
                      )
import Test.Tasty (testGroup, TestName, TestTree, defaultMain)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

-- | Crude check to see if parse worked.
parseSuccess :: Either a b -> Bool
parseSuccess (Left _) = False
parseSuccess _        = True

parseFailure :: Either a b -> Bool
parseFailure = not . parseSuccess

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Compile empty unit" $ assert $ parseSuccess (compileUnit "")
  , testCase "Compile whitespace-only unit" $ assert $ parseSuccess (compileUnit "  \n    ")

  , testCase "Counting all nodes" $
    7 @=? let
        tree = Unit
          [ FunctionDeclaration True
                              "name"
                              (ArgumentList [])
                              []
                              [UnletStatement True "foo"]
          , DocBlock [ HeadingAnnotation "foo"
                    , SubheadingAnnotation "bar"
                    , SubheadingAnnotation "baz"
                    ]
          ]
        counter _ = 1
        nodeCount = getSum $ walk counter (Sum 0) tree
      in nodeCount

  , testCase "Gathering specific nodes" $
    [SubheadingAnnotation "bar", SubheadingAnnotation "baz"] @=? let
        tree = DocBlock [ HeadingAnnotation "foo"
                        , SubheadingAnnotation "bar"
                        , SubheadingAnnotation "baz"
                        ]
        accumulateSubheadings node@(SubheadingAnnotation _) = [node]
        accumulateSubheadings _ = [] -- skip everything else
        selection = walk accumulateSubheadings [] tree
      in selection

  , testCase "Extracting symbols" $
    sort ["foo", "bar", "baz"] @=? let
        tree = DocBlock [ LinkTargets ["foo"]
                        , LinkTargets ["bar", "baz"]
                        ]
        symbols = sort $ getSymbols tree
      in symbols

  , testCase "Synthesizing symbols from the @plugin annotation" $
    sort ["foo", "foo.txt", "bar"] @=? let
        tree = DocBlock [ PluginAnnotation "foo" "some plugin"
                        , LinkTargets ["bar"]
                        ]
        symbols = sort $ getSymbols tree
      in symbols

  , testCase "Synthesizing symbols from the headings" $
    -- will need to pass in plugin name (prefix) to make this work
    sort ["foo", "foo.txt", "foo-history", "foo-troubleshooting-tips", "bar"] @=? let
        tree = DocBlock [ PluginAnnotation "foo" "some plugin"
                        , HeadingAnnotation "History"
                        , HeadingAnnotation "Troubleshooting tips"
                        , LinkTargets ["bar"]
                        ]
        symbols = sort $ getSymbols tree
      in symbols
  ]

goldenTests :: String -> [FilePath] -> (String -> String) -> TestTree
goldenTests description sources transform = testGroup groupName $ do
  file <- sources -- list monad
  let
    run = do
      input <- readFile file
      let output = normalize $ transform input
      return $ pack output -- pack because tasty-golden wants a ByteString
    name = takeBaseName file
    golden = replaceExtension file ".golden"
    diff ref new = [ "git"
                   , "diff"
                   , "--color"
                   , "--diff-algorithm=histogram"
                   , ref
                   , new
                   ]
  return $ goldenVsStringDiff' name diff golden run
  where
    groupName = "Golden " ++ description ++ " tests"

-- | Normalize a string to always end with a newline, unless zero-length, to
-- match standard text editor behavior.
normalize :: String -> String
normalize s | s == ""   = ""
            | otherwise = if last s == '\n' then s else s ++ "\n"

-- | This is based on `goldenVsStringDiff` function defined in:
-- https://github.com/feuerbach/tasty-golden/blob/470e7af018/Test/Tasty/Golden.hs#L150-L191
--
-- Differences:
--
--  - Omission of the verbose/ugly failure output message (this is the
--    motivating change here).
--  - Strip diff headers up to first "@@" (again, for brevity).
--  - Some revised names to make things a little clearer.
--  - Removed an `error` call which I am not worried about needing.
--
goldenVsStringDiff' :: TestName -> (FilePath -> FilePath -> [String]) -> FilePath -> IO LazyByteString.ByteString -> TestTree
goldenVsStringDiff' name diff golden run =
  goldenTest
    name
    (ByteString.readFile golden)
    (LazyByteString.toStrict <$> run)
    compare
    update
  where
    template = takeFileName golden <.> "actual"
    hunkHeader = map chr [0x1b, 0x5b, 0x33, 0x36, 0x6d] ++ "@@ "
    strip out = unlines $ dropWhile (not . isPrefixOf hunkHeader) (lines $ unpack out)
    compare _ actBS = withSystemTempFile template $ \tmpFile tmpHandle -> do
      ByteString.hPut tmpHandle actBS >> hFlush tmpHandle
      let cmd = diff golden tmpFile
      (_, Just sout, _, pid) <- createProcess (proc (head cmd) (tail cmd)) { std_out = CreatePipe }
      out <- LazyByteString.hGetContents sout
      evaluate . rnf $ out
      r <- waitForProcess pid
      return $ case r of
        ExitSuccess -> Nothing
        _ -> Just (strip out)
    update = ByteString.writeFile golden

getFixtures :: String -> IO [FilePath]
getFixtures = findByExtension [".vim"]

main :: IO ()
main = do
  parserSources <- getFixtures "tests/fixtures/parser"
  markdownSources <- getFixtures "tests/fixtures/markdown"
  vimHelpSources <- getFixtures "tests/fixtures/vim"
  defaultMain $ testGroup "Test suite"
    [ unitTests
    , goldenTests "parser" parserSources p
    , goldenTests "Markdown printer" markdownSources pm
    , goldenTests "Vim help printer" vimHelpSources pv
    ]
