-- | The docvim executable.
module Main (main) where

import Text.Docvim.CLI (run)

-- | Run the executable using the supplied options.
main :: IO ()
main = run
