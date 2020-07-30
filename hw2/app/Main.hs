module Main where

import Launcher (main')
import System.Directory
import System.Environment

-- This is the entry point to the program providing offline work with the file system
-- with support for local version control. To do this, transfer the start directory
-- from which the local file system will be initialized.
--
-- Example:
-- stack exec hw2 -- "/Users/nikita/hw2-niki999922/hw2/testFolder/Root"
--
main :: IO ()
main = do
  args <- getArgs :: IO [String]
  if (not $ null args)
  then do
    let workingDirector = (!!) args 0
    isExist <- doesPathExist workingDirector
    if (isExist)
    then do
      main' workingDirector
      return ()
    else putStrLn $ "Directory doesn't exist \"" ++ workingDirector ++ "\""
  else putStrLn $ "Can't find first argument\nFirst argument is absolute path to start working directory"
