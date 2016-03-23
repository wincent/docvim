module Main (main) where

import Docvim.Parse (p, parseUnit)
import System.FilePath (takeBaseName)
import System.IO (readFile)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
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
  file <- sources -- list monad (?)
  -- input <- readFile "tests/fixtures/parser/empty.vim" -- file
  let input = ""
  let output = return $ pack $ p input ++ "\n" -- goldenVsString wants a ByteString
  return $ goldenVsString (takeBaseName file) "tests/fixtures/parser/empty.golden" output

getFixtures :: IO [FilePath]
getFixtures = findByExtension [".vim"] "tests/fixtures/parser"

main :: IO ()
main = do
  sources <- getFixtures
  defaultMain $ testGroup "Test suite" [ unitTests
                                       , goldenTests sources
                                       ]
