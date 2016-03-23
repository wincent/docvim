module Main (main) where

import Docvim.Parse (p, parseUnit)
import System.FilePath (replaceExtension, takeBaseName)
import System.IO (readFile)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Test.Tasty.HUnit

import Data.ByteString.Lazy.Char8 (pack)

-- | Crude check to see if parse worked.
parseSuccess :: Either a b -> Bool
parseSuccess (Left _) = False
parseSuccess _        = True

parseFailure :: Either a b -> Bool
parseFailure = not . parseSuccess

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Parse empty unit" $ assert $ parseSuccess (parseUnit "")
  , testCase "Parse whitespace-only unit" $ assert $ parseSuccess (parseUnit "  \n    ")
  , testCase "Bad input" $ assert $ parseFailure (parseUnit "bzzzzt")

  -- Some example syntax:
  -- , testCase "Equality" $ True @=? True
  -- , testCase "Assertion" $ assert $ (length [1, 2, 3]) == 3
  ]

goldenTests :: [FilePath] -> TestTree
goldenTests sources = testGroup "Golden tests" $ do
  file <- sources -- list monad
  let
    run = do
      input <- readFile file
      let output = p input ++ "\n" -- Editors ensure the final newline.
      return $ pack output -- goldenVsString wants a ByteString
    name = takeBaseName file
    golden = replaceExtension file ".golden"
    diff = \ref new -> ["git", "diff", "--color", ref, new]
  return $ goldenVsStringDiff name diff golden run

getFixtures :: IO [FilePath]
getFixtures = findByExtension [".vim"] "tests/fixtures/parser"

main :: IO ()
main = do
  sources <- getFixtures
  defaultMain $ testGroup "Test suite" [ unitTests
                                       , goldenTests sources
                                       ]
