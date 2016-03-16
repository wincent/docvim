-- Allows us to `:load` modules in `ghci` that depend on Cabal's build-time
-- `Paths_docvim.hs` module (`dist/build/autogen/Paths_docvim.hs`).
--
-- https://mail.haskell.org/pipermail/haskell-cafe/2009-October/068058.html
module Paths_docvim (version) where

import Data.Version (Version(..))

version :: Version
version = Version [0, 0, 0, 0] []
