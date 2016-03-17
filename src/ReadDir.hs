-- | Recursively read the contents of Vim source files in a directory.
--
-- Based on `RecursiveContents` example in chapter 9 of "Real World Haskell".
module ReadDir (readDir) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

readDir :: FilePath -> IO [FilePath]
readDir topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then readDir path
      else return [path]
  return (concat paths)
