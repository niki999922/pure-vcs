{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Launcher where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Time.Clock
import System.Directory
import System.IO

import Directory
import Parser
import WorkingEnvironment

-- This is the beginning of the program, where at the beginning the status
-- of the file system and the control system is read,
-- and then there is work with it.
--
-- At the end, all new changes will be written to the initial directory.
--
main' :: FilePath -> IO ()
main' path = do
  state' <- readDirectoriesState path
  stateNew <- cycleRead $ WorkingEnvironment path [] state'
  _ <- removeDirectoryRecursive $ weStartPath stateNew
  _ <- createDirectory $ weStartPath stateNew
  _ <- dSaveDirectory $ weWorkAround stateNew
  return ()

-- For local testing
mainr :: IO WorkingEnvironment
mainr = do
  let path = "/Users/nikita/hw2-niki999922/hw2/TEST_DIR/timeDir"
  state' <- readDirectoriesState path
  let workingState = WorkingEnvironment path [] state'
  putStrLn "\n---------\nXXXXXXXXX\n---------\n"
  stateNew <- cycleRead workingState
  _ <- removeDirectoryRecursive $ weStartPath stateNew
  _ <- createDirectory $ weStartPath stateNew
  _ <- dSaveDirectory $ weWorkAround stateNew
  putStrLn "\n---------\nXXXXXXXXX\n---------\n"
  return stateNew

cycleRead :: WorkingEnvironment -> IO WorkingEnvironment
cycleRead we = do
  putStr $ (weGetCurrentDirectoryCMD we) ++ " > "
  hFlush stdout
  command <- getLine
  case (runParser commandParser command) of
    Just x -> case x of
      (CommandCD path, _) -> do
        newFs <- unpackStateExcept we $ weCommandCD path
        cycleRead newFs
      (CommandInformation path, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandInformation path
        cycleRead newFs
      (CommandFindFile path, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandFindFile path
        cycleRead newFs
      (CommandCat path, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandCat path
        cycleRead newFs
      (CommandCreateFolder path, _) -> do
        newFs <- unpackStateExcept we $ weCommandCreateFolder path
        cycleRead newFs
      (CommandCreateFile path, _) -> do
        curTime <- getCurrentTime
        newFs <- unpackStateExcept we $ weCommandCreateFile path curTime
        cycleRead newFs
      (CommandRemove path, _) -> do
        newFs <- unpackStateExcept we $ weCommandRemove path
        cycleRead newFs
      (CommandWriteFile path content, _) -> do
        curTime <- getCurrentTime
        newFs <- unpackStateExcept we $ weCommandWriteFile path content curTime
        cycleRead newFs
      (CommandDir, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandDir
        cycleRead newFs
      (CommandLS path, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandLS path
        cycleRead newFs
      (CommandCVSInit, _) -> do
        curTime <- getCurrentTime
        newFs <- unpackStateExcept we $ weCommandCVSInit curTime
        cycleRead newFs
      (CommandCVSAdd path, _) -> do
        curTime <- getCurrentTime
        newFs <- unpackStateExcept we $ weCommandCVSAdd path curTime
        cycleRead newFs
      (CommandCVSUpdate path text, _) -> do
        curTime <- getCurrentTime
        newFs <- unpackStateExcept we $ weCommandCVSUpdate path text curTime
        cycleRead newFs
      (CommandCVSHistory path, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandCVSHistory path
        cycleRead newFs
      (CommandCVSCat path version, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandCVSCat path version
        cycleRead newFs
      (CommandCVSMergeRevs path version1 version2 strategy, _) -> do
        curTime <- getCurrentTime
        newFs <- unpackStateExcept we $ weCommandCVSMergeRevs path version1 version2 strategy curTime
        cycleRead newFs
      (CommandCVSDeleteVersion path version, _) -> do
        newFs <- unpackStateExcept we $ weCommandCVSDeleteVersion path version
        cycleRead newFs
      (CommandCVSRemove path, _) -> do
        newFs <- unpackStateExcept we $ weCommandCVSRemove path
        cycleRead newFs
      (CommandCVSShowEverything, _) -> do
        newFs <- unpackStateExceptWithResult we $ weCommandCVSShowEverything
        cycleRead newFs
      (CommandHelp, _) -> do
        putStrLn helpText
        cycleRead we
      (CommandEmpty, _) -> cycleRead we
      (CommandExit, _) -> return we
    Nothing -> do
      putStrLn "Unknown command, for more information use \"help\""
      cycleRead we

unpackStateExcept :: WorkingEnvironment -> StateWE WorkingEnvironment IO () -> IO WorkingEnvironment
unpackStateExcept we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left errorText -> do
      putStrLn $ errorText
      return we
    Right eatherResNewFs -> return $ snd eatherResNewFs

unpackStateExceptWithResult :: WorkingEnvironment -> StateWE WorkingEnvironment IO String -> IO WorkingEnvironment
unpackStateExceptWithResult we st = do
  exceptResult <- runExceptT $ runStateT st we
  case exceptResult of
    Left errorText -> do
      putStrLn $ errorText
      return we
    Right eatherResNewFs -> do
      putStrLn $ fst eatherResNewFs
      return $ snd eatherResNewFs

helpText :: String
helpText = "cd <folder> -- перейти в директори\n" ++
           "dir -- показать содержимое текущей директори\n" ++
           "ls <folder> -- показать содержимое выбранной директори\n" ++
           "create-folder \"folder-name\" -- создать директорию в текуще\n" ++
           "cat <file> -- показать содержимое файл\n" ++
           "create-file \"file-name\" -- создать пустой файл в текущей директори\n" ++
           "remove <folder | file> -- удалить выборанную директорию или фай\n" ++
           "write-file <file> \"text\" -- записать текст в фай\n" ++
           "find-file \"file-name\" --  поиск файла в текущией директории и поддиректория\n" ++
           "information <file> -- показать информацию о файл\n" ++
           "information <folder> -- показать информацию о директори\n" ++
           "cvs-init -- инициализация СКВ в текущей выбранной директори\n" ++
           "cvs-add <file | folder> -- добавление файла или папки в СК\n" ++
           "cvs-update <file> \"comment\" -- добавление изменений файла в СК\n" ++
           "cvs-history <file> -- просмотр истории изменений файл\n" ++
           "cvs-cat <file> \"index\" -- просмотр конкретной ревизии файл\n" ++
           "cvs-merge-revs <file> \"index1\" \"index2\" \"left | right | both\" -\n" ++
           "объединение ревизий файла по заданным индексам, left, right, both или interactiv\n" ++
           "являются вариантами стратегий для обеъединени\n" ++
           "cvs-delete-version <file> \"index\" -- удалить заданную версию файла из ревизи\n" ++
           "cvs-remove <file> -- удалить файл из СК\n" ++
           "cvs-show-everything -- показать общую историю изменени\n" ++
           "help --  показать руководство по использовани\n" ++
           "exit -- завершение работы программы"
