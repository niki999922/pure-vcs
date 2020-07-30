module CommandTests
  ( workingEnvironmentCommandsTests,
    cvsCommandsTests
  ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Time.Clock
import Directory
import Launcher
import WorkingEnvironment

import Data.List
import Data.List.Split
import System.Directory

import Test.Hspec (SpecWith, describe, it, shouldBe)

workingEnvironmentCommandsTests :: SpecWith ()
workingEnvironmentCommandsTests =
  describe "Testing working CLI with local Working Environment" $ do
    it "tesing \"dir\" command" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCD "BigHouse/flat1")
      result <- unpackStateWithResult newState1 weCommandDir
      (result) `shouldBe` "Flat.txt\ncat.txt"

    it "tesing \"cd\" command OK down" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandCD "BigHouse/flat1")
      (weGetLocalPath newState) `shouldBe` "BigHouse/flat1"

    it "tesing \"cd\" command OK up" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCD "BigHouse/flat1")
      newState2 <- unpackStateExcept newState1 (weCommandCD "..")
      (weGetLocalPath newState2) `shouldBe` "BigHouse"

    it "tesing \"cd\" command OK up more than start directory" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCD "BigHouse/flat1")
      result <- unpackStateWithoutResult newState1 (weCommandCD "../../..")
      (result) `shouldBe` "Don't more than maximum length to root local FileSystem 3 > 2"

    it "tesing \"cd\" command BAD on multy \"..\" and down path" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCD "SMALL_House/flat1/../flat1")
      (result) `shouldBe` "Don't use multypath with \"..\", only \"..\" or without \"..\" in: \"[\"SMALL_House\",\"flat1\",\"..\",\"flat1\"]\""

    it "tesing \"cd\" command BAD on multy \"..\" and down path on non-exist directory" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCD "SMALL_House/flat1/a/sad/asd/../flat1")
      (result) `shouldBe` "Don't use multypath with \"..\", only \"..\" or without \"..\" in: \"[\"SMALL_House\",\"flat1\",\"a\",\"sad\",\"asd\",\"..\",\"flat1\"]\""

    it "tesing \"cd\" command BAD" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCD "SMALL_House/flat1")
      (result) `shouldBe` "\"SMALL_House/flat1\" does not exist"

    it "tesing \"ls\" command OK" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandLS "BigHouse/flat2")
      (result) `shouldBe` "Flat.txt"

    it "tesing \"ls\" command BAD" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandLS "SMALL_House/flat1")
      (result) `shouldBe` "\"SMALL_House/flat1\" does not exist"

    it "tesing \"create-folder\" command OK" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandCreateFolder "newFolder")
      result <- unpackStateWithResult newState (weCommandLS "newFolder")
      (result) `shouldBe` ""

    it "tesing \"create-folder\" command BAD on exist directory" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCreateFolder "BigHouse")
      (result) `shouldBe` "Can't create folder, this name is taken \"" ++ hw2Directory ++ "/testFolder/Root/BigHouse\""

    it "tesing \"create-folder\" command BAD on relative path" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCreateFolder "lol/kek/newFolder")
      (result) `shouldBe` "Can't create folder, this name contain relative path \"lol/kek/newFolder\""

    it "tesing \"create-file\" command OK" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandCreateFile "newfile.txt" curTime)
      result <- unpackStateWithResult newState weCommandDir
      (result) `shouldBe` "BigHouse\nEatFile.txt\nFileRoot.txt\nnewfile.txt"

    it "tesing \"create-file\" command BAD on exist file" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCreateFile "BigHouse/HouseHolder.txt" curTime)
      (result) `shouldBe` "Can't create file, this name contain relative path \"BigHouse/HouseHolder.txt\""

    it "tesing \"create-file\" command BAD on relative path" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCreateFile "lol/kek/newFile.txt" curTime)
      (result) `shouldBe` "Can't create file, this name contain relative path \"lol/kek/newFile.txt\""

    it "tesing \"cat\" command OK" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandCat "FileRoot.txt")
      (result) `shouldBe` "Hello world"

    it "tesing \"cat\" command OK on exist file in subdirectories" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandCat "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "Hello in big house"

    it "tesing \"cat\" command BAD on relative path" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandCat "lol/kek/newFile.txt")
      (result) `shouldBe` "Can't find directory with file: \"lol/kek\""

    it "tesing \"remove\" command OK file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandRemove "FileRoot.txt")
      result <- unpackStateWithResult newState weCommandDir
      (result) `shouldBe` "BigHouse\nEatFile.txt"

    it "tesing \"remove\" command OK on exist file in subdirectories" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandRemove "BigHouse/HouseHolder.txt")
      result <- unpackStateWithResult newState (weCommandLS "BigHouse")
      (result) `shouldBe` "flat2\nflat1"

    it "tesing \"remove\" command OK on directory" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandRemove "BigHouse")
      result <- unpackStateWithResult newState weCommandDir
      (result) `shouldBe` "EatFile.txt\nFileRoot.txt"

    it "tesing \"remove\" command OK on subdirectories" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandRemove "BigHouse/flat1")
      result <- unpackStateWithResult newState (weCommandLS "BigHouse")
      (result) `shouldBe` "flat2\nHouseHolder.txt"

    it "tesing \"remove\" command BAD on non-exist file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandRemove "lol/kek/newFile.txt")
      (result) `shouldBe` "Can't find file or directory for removing: \"[\"lol\",\"kek\",\"newFile.txt\"]\""

    it "tesing \"remove\" command BAD on non-exist directory" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandRemove "lol/kek/newFolder")
      (result) `shouldBe` "Can't find file or directory for removing: \"[\"lol\",\"kek\",\"newFolder\"]\""

    it "tesing \"write-file\" command OK" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandWriteFile "FileRoot.txt" "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!" curTime)
      result <- unpackStateWithResult newState (weCommandCat "FileRoot.txt")
      (result) `shouldBe` "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!"

    it "tesing \"write-file\" command OK on relative path" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCD "BigHouse")
      newState2 <- unpackStateExcept newState1 (weCommandCreateFile "newFile.txt" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCD "..")
      newState4 <- unpackStateExcept newState3 (weCommandWriteFile "BigHouse/newFile.txt" "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!" curTime)
      result <- unpackStateWithResult newState4 (weCommandCat "BigHouse/newFile.txt")
      (result) `shouldBe` "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!"

    it "tesing \"write-file\" command OK on relative path existed file" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandWriteFile "BigHouse/HouseHolder.txt" "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!" curTime)
      result <- unpackStateWithResult newState (weCommandCat "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!"

    it "tesing \"write-file\" command BAD" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandWriteFile "newFile.txt" "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!" curTime)
      (result) `shouldBe` "Can't find file \"newFile.txt\""

    it "tesing \"write-file\" command BAD on non-existed file relative path" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandWriteFile "lol/kek/newFile.txt" "NARUTOOOOOOOOOO!!!!! SASUKE!!!!!!!!" curTime)
      (result) `shouldBe` "Can't find file \"lol/kek/newFile.txt\""

    it "tesing \"find-file\" command OK" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandFindFile "FileRoot.txt")
      (init (drop (length hw2Directory + 2) result)) `shouldBe` "testFolder/Root/FileRoot.txt"

    it "tesing \"find-file\" command OK in subdirectories" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandFindFile "HouseHolder.txt")
      (init (drop (length hw2Directory + 2) result)) `shouldBe` "testFolder/Root/BigHouse/HouseHolder.txt"

    it "tesing \"find-file\" command BAD on non-existed file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandFindFile "newFile.txt")
      (result) `shouldBe` "File not found: \"newFile.txt\""

    it "tesing \"information\" command OK for file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandInformation "EatFile.txt")
      let listResults = splitString result
      let newPath = (drop (length hw2Directory + 1) (init (drop 7 (head listResults))))
      let newFirstElementList = "Path: \"" ++ newPath ++ "\""
      (intercalate "\n" $ [newFirstElementList] ++ (tail (take 3 listResults)) ++ (drop 4 listResults)) `shouldBe` "Path: \"testFolder/Root/EatFile.txt\"\nPermissions: \"rw\"\nExtension: \".txt\"\nSize: 13 bytes\n"

    it "tesing \"information\" command OK for file in subdirictories" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandInformation "BigHouse/flat1/cat.txt")
      let listResults = splitString result
      let newPath = (drop (length hw2Directory + 1) (init (drop 7 (head listResults))))
      let newFirstElementList = "Path: \"" ++ newPath ++ "\""
      (intercalate "\n" $ [newFirstElementList] ++ (tail (take 3 listResults)) ++ (drop 4 listResults)) `shouldBe` "Path: \"testFolder/Root/BigHouse/flat1/cat.txt\"\nPermissions: \"rw\"\nExtension: \".txt\"\nSize: 22 bytes\n"

    it "tesing \"information\" command BAD on non-existed file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandInformation "newFile.txt")
      (result) `shouldBe` "Can't find file or directory \"newFile.txt\""

    it "tesing \"information\" command OK for folder" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandInformation "BigHouse")
      let listResults = splitString result
      let newPath = (drop (length hw2Directory + 1) (init (drop 7 (head listResults))))
      let newFirstElementList = "Path: \"" ++ newPath ++ "\""
      (intercalate "\n" $ [newFirstElementList] ++ (tail (take 3 listResults)) ++ (drop 4 listResults)) `shouldBe` "Path: \"testFolder/Root/BigHouse\"\nPermissions: \"rws\"\nSize: 69 bytes\n"

    it "tesing \"information\" command OK for folder in subdirictories" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandInformation "BigHouse/flat1")
      let listResults = splitString result
      let newPath = (drop (length hw2Directory + 1) (init (drop 7 (head listResults))))
      let newFirstElementList = "Path: \"" ++ newPath ++ "\""
      (intercalate "\n" $ [newFirstElementList] ++ (tail (take 3 listResults)) ++ (drop 4 listResults)) `shouldBe` "Path: \"testFolder/Root/BigHouse/flat1\"\nPermissions: \"rws\"\nSize: 41 bytes\n"

    it "tesing \"information\" command BAD on non-existed folder" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandInformation "BigHouse/flat1999")
      (result) `shouldBe` "Can't find file or directory \"BigHouse/flat1999\""


cvsCommandsTests :: SpecWith ()
cvsCommandsTests =
  describe "Testing working CLI with local CVS" $ do
    it "tesing \"cvs-init\" command OK" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSInit curTime)
      (result) `shouldBe` ""

    it "tesing \"cvs-add\" command OK for file" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      result <- unpackStateWithResult newState (weCommandCVSCat "EatFile.txt" "0")
      (result) `shouldBe` "I want to eat"

    it "tesing \"cvs-add\" command OK for file in subdirectories" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      result <- unpackStateWithResult newState (weCommandCVSCat "BigHouse/HouseHolder.txt" "0")
      (result) `shouldBe` "Hello in big house"

    it "tesing \"cvs-add\" command BAD on non-existed file" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSAdd "BigHouse/HouseBolder.txt" curTime)
      (result) `shouldBe` "Can't find file or directory for adding in CVS: \"BigHouse/HouseBolder.txt\""

    it "tesing \"cvs-add\" command OK for folder" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSAdd "BigHouse" curTime)
      (result) `shouldBe` ""

    it "tesing \"cvs-add\" command OK for folder in subdirectories" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSAdd "BigHouse/flat1" curTime)
      (result) `shouldBe` ""

    it "tesing \"cvs-add\" command BAD on non-existed folder" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSAdd "BigHouse/HouseFocus" curTime)
      (result) `shouldBe` "Can't find file or directory for adding in CVS: \"BigHouse/HouseFocus\""

    it "tesing \"cvs-update\" command OK" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit" curTime)
      result <- unpackStateWithResult newState3 (weCommandCVSCat "EatFile.txt" "1")
      (result) `shouldBe` "NEW TEXT LOL"

    it "tesing \"cvs-update\" command OK in subdirectories" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit" curTime)
      result <- unpackStateWithResult newState3 (weCommandCVSCat "BigHouse/HouseHolder.txt" "1")
      (result) `shouldBe` "NEW TEXT LOL"

    it "tesing \"cvs-update\" command BAD on non-untraced file" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "new text kek" curTime)
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked"

    it "tesing \"cvs-history\" command OK" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit 2" curTime)
      result <- unpackStateWithResult newState3 (weCommandCVSHistory "EatFile.txt")
      (result) `shouldBe` "0. initial\n1. wow commit 1\n2. wow commit 2"

    it "tesing \"cvs-history\" command OK in subdirectories" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 2" curTime)
      result <- unpackStateWithResult newState3 (weCommandCVSHistory "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "0. initial\n1. wow commit 1\n2. wow commit 2"

    it "tesing \"cvs-history\" command BAD on non-untraced file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandCVSHistory "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked"

    it "tesing \"cvs-cat\" command OK " $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      result <- unpackStateWithResult newState2 (weCommandCVSCat "EatFile.txt" "1")
      (result) `shouldBe` "I want to eat"

    it "tesing \"cvs-cat\" command OK in subdirectories" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      result <- unpackStateWithResult newState2 (weCommandCVSCat "BigHouse/HouseHolder.txt" "1")
      (result) `shouldBe` "Hello in big house"

    it "tesing \"cvs-cat\" command BAD on non-untraced file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithResult workingState (weCommandCVSCat "BigHouse/HouseHolder.txt" "1")
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked"

    it "tesing \"cvs-merge-revs\" command OK with \"left\" strategy" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSMergeRevs "EatFile.txt" "0" "1" "left" curTime)
      result <- unpackStateWithResult newState4 (weCommandCVSCat "EatFile.txt" "2")
      (result) `shouldBe` "I want to eat"

    it "tesing \"cvs-merge-revs\" command OK with \"right\" strategy" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSMergeRevs "EatFile.txt" "0" "1" "right" curTime)
      result <- unpackStateWithResult newState4 (weCommandCVSCat "EatFile.txt" "2")
      (result) `shouldBe` "NEW TEXT LOL!!)"

    it "tesing \"cvs-merge-revs\" command OK with \"both\" strategy" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSMergeRevs "EatFile.txt" "0" "1" "both" curTime)
      result <- unpackStateWithResult newState4 (weCommandCVSCat "EatFile.txt" "2")
      (result) `shouldBe` "I want to eat\n>>>>\nNEW TEXT LOL!!)"

    it "tesing \"cvs-merge-revs\" command OK in subdirectory with \"left\" strategy" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSMergeRevs "BigHouse/HouseHolder.txt" "0" "1" "left" curTime)
      result <- unpackStateWithResult newState4 (weCommandCVSCat "BigHouse/HouseHolder.txt" "2")
      (result) `shouldBe` "Hello in big house"

    it "tesing \"cvs-merge-revs\" command OK in subdirectory  with \"right\" strategy" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSMergeRevs "BigHouse/HouseHolder.txt" "0" "1" "right" curTime)
      result <- unpackStateWithResult newState4 (weCommandCVSCat "BigHouse/HouseHolder.txt" "2")
      (result) `shouldBe` "NEW TEXT LOL!!)"

    it "tesing \"cvs-merge-revs\" command OK in subdirectory  with \"both\" strategy" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSMergeRevs "BigHouse/HouseHolder.txt" "0" "1" "both" curTime)
      result <- unpackStateWithResult newState4 (weCommandCVSCat "BigHouse/HouseHolder.txt" "2")
      (result) `shouldBe` "Hello in big house\n>>>>\nNEW TEXT LOL!!)"

    it "tesing \"cvs-merge-revs\" command BAD without left commit" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      result <- unpackStateWithoutResult newState3 (weCommandCVSMergeRevs "EatFile.txt" "213" "1" "davayUse" curTime)
      (result) `shouldBe` "Commit \"213\" not found"

    it "tesing \"cvs-merge-revs\" command BAD without rigth commit" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      result <- unpackStateWithoutResult newState3 (weCommandCVSMergeRevs "EatFile.txt" "0" "1123" "davayUse" curTime)
      (result) `shouldBe` "Commit \"1123\" not found"

    it "tesing \"cvs-merge-revs\" command BAD without both commit" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow commit 1" curTime)
      result <- unpackStateWithoutResult newState3 (weCommandCVSMergeRevs "EatFile.txt" "1320" "1123" "davayUse" curTime)
      (result) `shouldBe` "Commit \"1320\" not found"

    it "tesing \"cvs-merge-revs\" command BAD with untraced file" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSMergeRevs "EatFile.txt" "0" "1" "left" curTime)
      (result) `shouldBe` "File \"EatFile.txt\" is untracked"

    it "tesing \"cvs-merge-revs\" command BAD with incorrect file name" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSMergeRevs "RunFile.txt" "0" "1" "left" curTime)
      (result) `shouldBe` "Can't find file \"RunFile.txt\""

    it "tesing \"cvs-merge-revs\" command BAD in subdirictories without left commit" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      result <- unpackStateWithoutResult newState3 (weCommandCVSMergeRevs "BigHouse/HouseHolder.txt" "213" "1" "davayUse" curTime)
      (result) `shouldBe` "Commit \"213\" not found"

    it "tesing \"cvs-merge-revs\" command BAD in subdirictories without rigth commit" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      result <- unpackStateWithoutResult newState3 (weCommandCVSMergeRevs "BigHouse/HouseHolder.txt" "0" "1123" "davayUse" curTime)
      (result) `shouldBe` "Commit \"1123\" not found"

    it "tesing \"cvs-merge-revs\" command BAD in subdirictories without both commit" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow commit 1" curTime)
      result <- unpackStateWithoutResult newState3 (weCommandCVSMergeRevs "BigHouse/HouseHolder.txt" "1320" "1123" "davayUse" curTime)
      (result) `shouldBe` "Commit \"1320\" not found"

    it "tesing \"cvs-merge-revs\" command BAD in subdirictories with untraced file" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSMergeRevs "BigHouse/HouseHolder.txt" "0" "1" "left" curTime)
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked"

    it "tesing \"cvs-merge-revs\" command BAD in subdirictories with incorrect file name" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSMergeRevs "RunFile.txt" "0" "1" "left" curTime)
      (result) `shouldBe` "Can't find file \"RunFile.txt\""

    it "tesing \"cvs-delete-version\" command OK" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow only commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSDeleteVersion "EatFile.txt" "0")
      result <- unpackStateWithResult newState4 (weCommandCVSHistory "EatFile.txt")
      (result) `shouldBe` "1. wow only commit 1"

    it "tesing \"cvs-delete-version\" command OK no this version" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "EatFile.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "EatFile.txt" "wow only commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSDeleteVersion "EatFile.txt" "3")
      result <- unpackStateWithResult newState4 (weCommandCVSHistory "EatFile.txt")
      (result) `shouldBe` "0. initial\n1. wow only commit 1"

    it "tesing \"cvs-delete-version\" command OK in subdirectories" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow only commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSDeleteVersion "BigHouse/HouseHolder.txt" "0")
      result <- unpackStateWithResult newState4 (weCommandCVSHistory "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "1. wow only commit 1"

    it "tesing \"cvs-delete-version\" command OK in subdirectories no this version" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandWriteFile "BigHouse/HouseHolder.txt" "NEW TEXT LOL!!)" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow only commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSDeleteVersion "BigHouse/HouseHolder.txt" "3")
      result <- unpackStateWithResult newState4 (weCommandCVSHistory "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "0. initial\n1. wow only commit 1"

    it "tesing \"cvs-delete-version\" command BAD untraced file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSDeleteVersion "BigHouse/HouseHolder.txt" "0")
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked"

    it "tesing \"cvs-delete-version\" command BAD in subdirectories untraced file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSDeleteVersion "BigHouse/HouseHolder.txt" "0")
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked"

    it "tesing \"cvs-remove\" command OK" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "EatFile.txt" curTime)
      newState2 <- unpackStateExcept newState1 (weCommandCVSUpdate "EatFile.txt" "wow only commit 1" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCVSRemove "EatFile.txt")
      result <- unpackStateWithResult newState3 (weCommandCVSHistory "EatFile.txt")
      (result) `shouldBe` "File \"EatFile.txt\" is untracked"

    it "tesing \"cvs-remove\" command OK in subdirectory" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCVSAdd "BigHouse/HouseHolder.txt" curTime)
      newState3 <- unpackStateExcept newState1 (weCommandCVSUpdate "BigHouse/HouseHolder.txt" "wow only commit 1" curTime)
      newState4 <- unpackStateExcept newState3 (weCommandCVSRemove "BigHouse/HouseHolder.txt")
      result <- unpackStateWithResult newState4 (weCommandCVSHistory "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked"

    it "tesing \"cvs-remove\" command BAD" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSRemove "EatFile.txt")
      (result) `shouldBe` "File \"EatFile.txt\" is untracked yet"

    it "tesing \"cvs-remove\" command BAD unknown file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSRemove "Noname.txt")
      (result) `shouldBe` "Can't find file \"Noname.txt\""

    it "tesing \"cvs-remove\" command BAD in subdirectory" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSRemove "BigHouse/HouseHolder.txt")
      (result) `shouldBe` "File \"BigHouse/HouseHolder.txt\" is untracked yet"

    it "tesing \"cvs-remove\" command BAD in subdirectory unknown file" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      result <- unpackStateWithoutResult workingState (weCommandCVSRemove "BigHouse/NonameHolder.txt")
      (result) `shouldBe` "Can't find file \"BigHouse/NonameHolder.txt\""

    it "tesing \"cvs-show-everything\" command OK for init directory" $ do
      curTime <- getCurrentTime
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCD "BigHouse")
      newState2 <- unpackStateExcept newState1 (weCommandCVSAdd "flat1" curTime)
      newState3 <- unpackStateExcept newState2 (weCommandCD "flat1")
      result <- unpackStateWithResult newState3 weCommandCVSShowEverything
      (result) `shouldBe` "\"Flat.txt\"\n0. initial\n\"cat.txt\"\n0. initial\n"

    it "tesing \"cvs-show-everything\" command OK for non-init directory" $ do
      hw2Directory <- getCurrentDirectory
      let testFolderPath = getTestFolder hw2Directory
      weState <- readDirectoriesState testFolderPath
      let workingState = WorkingEnvironment testFolderPath [] weState
      newState1 <- unpackStateExcept workingState (weCommandCD "BigHouse/flat1")
      result <- unpackStateWithResult newState1 weCommandCVSShowEverything
      (result) `shouldBe` ""

unpackStateWithResult :: WorkingEnvironment -> StateWE WorkingEnvironment IO String -> IO String
unpackStateWithResult we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left errorText -> do
      return errorText
    Right (eatherResNewFs, _) -> do
      return eatherResNewFs

unpackStateWithoutResult :: WorkingEnvironment -> StateWE WorkingEnvironment IO () -> IO String
unpackStateWithoutResult we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left errorText -> do
      return errorText
    Right (_, _) -> return $ ""

getTestFolder :: String -> String
getTestFolder prefix = prefix ++ "/testFolder/Root"

splitString :: String -> [String]
splitString text = splitOn "\n" text
