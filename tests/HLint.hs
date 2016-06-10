module Main (main) where

import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = do
  hints <- hlint ["lib", "src"]
  if null hints then exitSuccess else exitFailure
