module ParserTests where

import Parser

import Test.Hspec (SpecWith, describe, it, shouldBe)

parserCorrectTests :: SpecWith ()
parserCorrectTests =
  describe "Testing working Parser commands on correct data" $ do
    it "\"help\" command" $ do
      let (Just (result, _)) = runParser commandParser "help   "
      result `shouldBe` CommandHelp
    it "\"cd\" command" $ do
      let (Just (result, _)) = runParser commandParser "cd needFolder/folder/file"
      result `shouldBe` CommandCD "needFolder/folder/file"
    it "\"cd\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "cd needFolder/folder/file"
      let (CommandCD arg1) = result
      arg1 `shouldBe` "needFolder/folder/file"
    it "\"information\" command" $ do
      let (Just (result, _)) = runParser commandParser "information needFolder/folder/file"
      result `shouldBe` CommandInformation "needFolder/folder/file"
    it "\"information\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "information needFolder/folder/file"
      let (CommandInformation arg1) = result
      arg1 `shouldBe` "needFolder/folder/file"
    it "\"find-file\" command" $ do
      let (Just (result, _)) = runParser commandParser "find-file \"file.txt\""
      result `shouldBe` CommandFindFile "file.txt"
    it "\"find-file\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "find-file \"file.txt\""
      let (CommandFindFile arg1) = result
      arg1 `shouldBe` "file.txt"
    it "\"cat\" command" $ do
      let (Just (result, _)) = runParser commandParser "cat file.txt"
      result `shouldBe` CommandCat "file.txt"
    it "\"cat\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "cat file.txt"
      let (CommandCat arg1) = result
      arg1 `shouldBe` "file.txt"
    it "\"create-folder\" command" $ do
      let (Just (result, _)) = runParser commandParser "create-folder \"folderName\""
      result `shouldBe` CommandCreateFolder "folderName"
    it "\"create-folder\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "create-folder \"folderName\""
      let (CommandCreateFolder arg1) = result
      arg1 `shouldBe` "folderName"
    it "\"create-file\" command" $ do
      let (Just (result, _)) = runParser commandParser "create-file \"fileName\""
      result `shouldBe` CommandCreateFile "fileName"
    it "\"create-file\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "create-file \"fileName\""
      let (CommandCreateFile arg1) = result
      arg1 `shouldBe` "fileName"
    it "\"remove\" command on file" $ do
      let (Just (result, _)) = runParser commandParser "remove folder/fileName.txt"
      result `shouldBe` CommandRemove "folder/fileName.txt"
    it "\"remove\" command arguments on file" $ do
      let (Just (result, _)) = runParser commandParser "remove folder/fileName.txt"
      let (CommandRemove arg1) = result
      arg1 `shouldBe` "folder/fileName.txt"
    it "\"remove\" command on foldare" $ do
      let (Just (result, _)) = runParser commandParser "remove folder/foldareName"
      result `shouldBe` CommandRemove "folder/foldareName"
    it "\"remove\" command arguments on foldare" $ do
      let (Just (result, _)) = runParser commandParser "remove folder/foldareName"
      let (CommandRemove arg1) = result
      arg1 `shouldBe` "folder/foldareName"
    it "\"write-file\" command on file" $ do
      let (Just (result, _)) = runParser commandParser "write-file fileName.txt \"NARUTOOOOOO\""
      result `shouldBe` CommandWriteFile "fileName.txt" "NARUTOOOOOO"
    it "\"write-file\" command arguments on file" $ do
      let (Just (result, _)) = runParser commandParser "write-file fileName.txt \"NARUTOOOOOO\""
      let (CommandWriteFile arg1 arg2) = result
      arg1 `shouldBe` "fileName.txt"
      arg2 `shouldBe` "NARUTOOOOOO"
    it "\"write-file\" command on file" $ do
      let (Just (result, _)) = runParser commandParser "write-file folder/fileName.txt \"NARUTOOOOOO\""
      result `shouldBe` CommandWriteFile "folder/fileName.txt" "NARUTOOOOOO"
    it "\"write-file\" command arguments on file" $ do
      let (Just (result, _)) = runParser commandParser "write-file folder/fileName.txt \"NARUTOOOOOO\""
      let (CommandWriteFile arg1 arg2) = result
      arg1 `shouldBe` "folder/fileName.txt"
      arg2 `shouldBe` "NARUTOOOOOO"
    it "\"dir\" command" $ do
      let (Just (result, _)) = runParser commandParser "dir   "
      result `shouldBe` CommandDir
    it "\"ls\" command" $ do
      let (Just (result, _)) = runParser commandParser "ls folder/path"
      result `shouldBe` CommandLS "folder/path"
    it "\"ls\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "ls folder/path"
      let (CommandLS arg1) = result
      arg1 `shouldBe` "folder/path"
    it "\"exit\" command" $ do
      let (Just (result, _)) = runParser commandParser "exit   "
      result `shouldBe` CommandExit
    it "empty command" $ do
      let (Just (result, _)) = runParser commandParser ""
      result `shouldBe` CommandEmpty
    it "\"cvs-init\" command" $ do
      let (Just (result, _)) = runParser commandParser "cvs-init   "
      result `shouldBe` CommandCVSInit
    it "\"cvs-add\" command on file" $ do
      let (Just (result, _)) = runParser commandParser "cvs-add folderName"
      result `shouldBe` CommandCVSAdd "folderName"
    it "\"cvs-add\" command arguments on file" $ do
      let (Just (result, _)) = runParser commandParser "cvs-add folderName"
      let (CommandCVSAdd arg1) = result
      arg1 `shouldBe` "folderName"
    it "\"cvs-add\" command on foldare" $ do
      let (Just (result, _)) = runParser commandParser "cvs-add fileName"
      result `shouldBe` CommandCVSAdd "fileName"
    it "\"cvs-add\" command arguments on foldare" $ do
      let (Just (result, _)) = runParser commandParser "cvs-add fileName"
      let (CommandCVSAdd arg1) = result
      arg1 `shouldBe` "fileName"
    it "\"cvs-update\" command" $ do
      let (Just (result, _)) = runParser commandParser "cvs-update fileName.txt \"NARUTOOOOOO\""
      result `shouldBe` CommandCVSUpdate "fileName.txt" "NARUTOOOOOO"
    it "\"cvs-update\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "cvs-update fileName.txt \"NARUTOOOOOO\""
      let (CommandCVSUpdate arg1 arg2) = result
      arg1 `shouldBe` "fileName.txt"
      arg2 `shouldBe` "NARUTOOOOOO"
    it "\"cvs-update\" command in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-update folder/fileName.txt \"NARUTOOOOOO\""
      result `shouldBe` CommandCVSUpdate "folder/fileName.txt" "NARUTOOOOOO"
    it "\"cvs-update\" command arguments in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-update folder/fileName.txt \"NARUTOOOOOO\""
      let (CommandCVSUpdate arg1 arg2) = result
      arg1 `shouldBe` "folder/fileName.txt"
      arg2 `shouldBe` "NARUTOOOOOO"
    it "\"cvs-history\" command on file" $ do
      let (Just (result, _)) = runParser commandParser "cvs-history folderName"
      result `shouldBe` CommandCVSHistory "folderName"
    it "\"cvs-history\" command arguments on file" $ do
      let (Just (result, _)) = runParser commandParser "cvs-history folderName"
      let (CommandCVSHistory arg1) = result
      arg1 `shouldBe` "folderName"
    it "\"cvs-history\" command on foldare" $ do
      let (Just (result, _)) = runParser commandParser "cvs-history fileName"
      result `shouldBe` CommandCVSHistory "fileName"
    it "\"cvs-history\" command arguments on foldare" $ do
      let (Just (result, _)) = runParser commandParser "cvs-history fileName"
      let (CommandCVSHistory arg1) = result
      arg1 `shouldBe` "fileName"
    it "\"cvs-cat\" command" $ do
      let (Just (result, _)) = runParser commandParser "cvs-cat fileName.txt \"NARUTOOOOOO\""
      result `shouldBe` CommandCVSCat "fileName.txt" "NARUTOOOOOO"
    it "\"cvs-cat\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "cvs-cat fileName.txt \"NARUTOOOOOO\""
      let (CommandCVSCat arg1 arg2) = result
      arg1 `shouldBe` "fileName.txt"
      arg2 `shouldBe` "NARUTOOOOOO"
    it "\"cvs-cat\" command in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-cat folder/fileName.txt \"NARUTOOOOOO\""
      result `shouldBe` CommandCVSCat "folder/fileName.txt" "NARUTOOOOOO"
    it "\"cvs-cat\" command arguments in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-cat folder/fileName.txt \"NARUTOOOOOO\""
      let (CommandCVSCat arg1 arg2) = result
      arg1 `shouldBe` "folder/fileName.txt"
      arg2 `shouldBe` "NARUTOOOOOO"
    it "\"cvs-merge-revs\" command" $ do
      let (Just (result, _)) = runParser commandParser "cvs-merge-revs fileName.txt \"1\" \"2\" \"left\""
      result `shouldBe` CommandCVSMergeRevs "fileName.txt" "1" "2" "left"
    it "\"cvs-merge-revs\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "cvs-merge-revs fileName.txt \"1\" \"2\" \"left\""
      let (CommandCVSMergeRevs arg1 arg2 arg3 arg4) = result
      arg1 `shouldBe` "fileName.txt"
      arg2 `shouldBe` "1"
      arg3 `shouldBe` "2"
      arg4 `shouldBe` "left"
    it "\"cvs-merge-revs\" command in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-merge-revs folder/fileName.txt \"1\" \"2\" \"left\""
      result `shouldBe` CommandCVSMergeRevs "folder/fileName.txt" "1" "2" "left"
    it "\"cvs-merge-revs\" command arguments in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-merge-revs folder/fileName.txt \"1\" \"2\" \"left\""
      let (CommandCVSMergeRevs arg1 arg2 arg3 arg4) = result
      arg1 `shouldBe` "folder/fileName.txt"
      arg2 `shouldBe` "1"
      arg3 `shouldBe` "2"
      arg4 `shouldBe` "left"
    it "\"cvs-delete-version\" command " $ do
      let (Just (result, _)) = runParser commandParser "cvs-delete-version fileName.txt \"1\""
      result `shouldBe` CommandCVSDeleteVersion "fileName.txt" "1"
    it "\"cvs-delete-version\" command arguments " $ do
      let (Just (result, _)) = runParser commandParser "cvs-delete-version fileName.txt \"1\""
      let (CommandCVSDeleteVersion arg1 arg2) = result
      arg1 `shouldBe` "fileName.txt"
      arg2 `shouldBe` "1"
    it "\"cvs-delete-version\" command in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-delete-version folder/fileName.txt \"1\""
      result `shouldBe` CommandCVSDeleteVersion "folder/fileName.txt" "1"
    it "\"cvs-delete-version\" command arguments in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-delete-version folder/fileName.txt \"1\""
      let (CommandCVSDeleteVersion arg1 arg2) = result
      arg1 `shouldBe` "folder/fileName.txt"
      arg2 `shouldBe` "1"
    it "\"cvs-remove\" command" $ do
      let (Just (result, _)) = runParser commandParser "cvs-remove folderName"
      result `shouldBe` CommandCVSRemove "folderName"
    it "\"cvs-remove\" command arguments" $ do
      let (Just (result, _)) = runParser commandParser "cvs-remove folderName"
      let (CommandCVSRemove arg1) = result
      arg1 `shouldBe` "folderName"
    it "\"cvs-remove\" command in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-remove fileName"
      result `shouldBe` CommandCVSRemove "fileName"
    it "\"cvs-remove\" command arguments in subdirectories" $ do
      let (Just (result, _)) = runParser commandParser "cvs-remove fileName"
      let (CommandCVSRemove arg1) = result
      arg1 `shouldBe` "fileName"
    it "\"cvs-show-everything\" command" $ do
      let (Just (result, _)) = runParser commandParser "cvs-show-everything   "
      result `shouldBe` CommandCVSShowEverything

parserIncorrectTests :: SpecWith ()
parserIncorrectTests =
  describe "Testing working Parser commands on uncorrect data" $ do
    it "\"cd\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "cd       "
      let (CommandCD arg1) = result
      arg1 `shouldBe` ""
    it "\"cd\" command non-empty" $ do
      let result = runParser commandParser "cd   random String?   "
      result `shouldBe` Nothing
    it "\"information\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "information       "
      let (CommandInformation arg1) = result
      arg1 `shouldBe` ""
    it "\"information\" command non-empty" $ do
      let result = runParser commandParser "information   random String?    "
      result `shouldBe` Nothing
    it "\"find-file\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "find-file       "
      let (CommandFindFile arg1) = result
      arg1 `shouldBe` ""
    it "\"find-file\" command non-empty" $ do
      let result = runParser commandParser "find-file   random String?    "
      result `shouldBe` Nothing
    it "\"cat\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "cat       "
      let (CommandCat arg1) = result
      arg1 `shouldBe` ""
    it "\"cat\" command non-empty" $ do
      let result = runParser commandParser "cat  random String?     "
      result `shouldBe` Nothing
    it "\"create-folder\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "create-folder       "
      let (CommandCreateFolder arg1) = result
      arg1 `shouldBe` ""
    it "\"create-folder\" command non-empty" $ do
      let result = runParser commandParser "create-folder  random String?     "
      result `shouldBe` Nothing
    it "\"create-file\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "create-file       "
      let (CommandCreateFile arg1) = result
      arg1 `shouldBe` ""
    it "\"create-file\" command non-empty" $ do
      let result = runParser commandParser "create-file    random String?   "
      result `shouldBe` Nothing
    it "\"remove\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "remove       "
      let (CommandRemove arg1) = result
      arg1 `shouldBe` ""
    it "\"remove\" command non-empty" $ do
      let result = runParser commandParser "remove    random String?   "
      result `shouldBe` Nothing
    it "\"write-file\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "write-file       "
      let (CommandWriteFile arg1 arg2) = result
      arg1 `shouldBe` ""
      arg2 `shouldBe` ""
    it "\"write-file\" command non-empty" $ do
      let result = runParser commandParser "write-file    random String? What?  "
      result `shouldBe` Nothing
    it "\"dir\" command non-empty" $ do
      let result = runParser commandParser "dir    random String?   "
      result `shouldBe` Nothing
    it "\"ls\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "ls       "
      let (CommandLS arg1) = result
      arg1 `shouldBe` ""
    it "\"ls\" command non-empty" $ do
      let result = runParser commandParser "ls    random String?   "
      result `shouldBe` Nothing
    it "\"exit\" command non-empty" $ do
      let result = runParser commandParser "exit    random String?   "
      result `shouldBe` Nothing
    it "\"cvs-init\" command non-empty" $ do
      let result = runParser commandParser "cvs-init    random String?   "
      result `shouldBe` Nothing
    it "\"cvs-add\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "cvs-add       "
      let (CommandCVSAdd arg1) = result
      arg1 `shouldBe` ""
    it "\"cvs-add\" command non-empty" $ do
      let result = runParser commandParser "cvs-add    random String?   "
      result `shouldBe` Nothing
    it "\"cvs-update\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "cvs-update       "
      let (CommandCVSUpdate arg1 arg2) = result
      arg1 `shouldBe` ""
      arg2 `shouldBe` ""
    it "\"cvs-update\" command non-empty" $ do
      let result = runParser commandParser "cvs-update    random String?   "
      result `shouldBe` Nothing
    it "\"cvs-history\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "cvs-history       "
      let (CommandCVSHistory arg1) = result
      arg1 `shouldBe` ""
    it "\"cvs-history\" command non-empty" $ do
      let result = runParser commandParser "cvs-history    random String?   "
      result `shouldBe` Nothing
    it "\"cvs-cat\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "cvs-cat       "
      let (CommandCVSCat arg1 arg2) = result
      arg1 `shouldBe` ""
      arg2 `shouldBe` ""
    it "\"cvs-cat\" command non-empty" $ do
      let result = runParser commandParser "cvs-cat    random String?   "
      result `shouldBe` Nothing
    it "\"cvs-remove\" command empty" $ do
      let (Just (result, _)) = runParser commandParser "cvs-remove       "
      let (CommandCVSRemove arg1) = result
      arg1 `shouldBe` ""
    it "\"cvs-remove\" command non-empty" $ do
      let result = runParser commandParser "cvs-remove    random String?   "
      result `shouldBe` Nothing
    it "\"cvs-show-everything\" command non-empty" $ do
      let result = runParser commandParser "cvs-show-everything    random String?   "
      result `shouldBe` Nothing
