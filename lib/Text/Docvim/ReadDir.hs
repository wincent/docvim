-- | Recursively read the paths in a directory.
--
-- Based on `RecursiveContents` example in chapter 9 of "Real World Haskell".
module Text.Docvim.ReadDir (readDir) where

import Control.Monad
import System.Directory
import System.FilePath

readDir :: FilePath -> IO [FilePath]
readDir dir = do
  names <- getDirectoryContents dir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then readDir path
      else return [path]
  return (concat paths)
