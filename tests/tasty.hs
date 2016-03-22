module Main (main) where

import Docvim.Parse (parseUnit)
import Test.Tasty
import Test.Tasty.HUnit

-- | Crude check to see if parse worked.
parseSuccess :: Either a b -> Bool
parseSuccess (Left _) = False
parseSuccess _        = True

parseFailure :: Either a b -> Bool
parseFailure = not . parseSuccess

suite :: TestTree
suite = testGroup "Test suite"
  [ testGroup "Unit tests"
    [ testCase "Parse empty unit" $ assert $ parseSuccess (parseUnit "")
    , testCase "Parse whitespace-only unit" $ assert $ parseSuccess (parseUnit "  \n    ")
    , testCase "Bad input" $ assert $ parseFailure (parseUnit "bzzzzt")

    -- Some example syntax:
    -- , testCase "Equality" $ True @=? True
    -- , testCase "Assertion" $ assert $ (length [1, 2, 3]) == 3
    ]
  ]

main :: IO ()
main = defaultMain suite
