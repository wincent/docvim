module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite = testGroup "Test suite"
  [ testGroup "Unit tests"
    [ testCase "Equality" $ True @=? True
    , testCase "Assertion" $ assert $ (length [1, 2, 3]) == 3
    ]
  ]

main :: IO ()
main = defaultMain suite
