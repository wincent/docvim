-- | The docvim executable.
module Main (main) where

import Options (Options(..), options)

run :: Options -> IO ()
run (Options (Just a) _ _ _) = putStrLn ("got file " ++ a)
run (Options _ False d _) = putStrLn "debugging off"
run (Options _ True d _) = putStrLn "debugging on"

main :: IO ()
main = options >>= run
