module Main
  ( main
  ) where

import CommandTests
import ParserTests
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  workingEnvironmentCommandsTests
  cvsCommandsTests
  parserCorrectTests
  parserIncorrectTests