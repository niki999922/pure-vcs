{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module WorkingEnvironment where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time.Clock
import System.FilePath.Posix

import Directory
import File

-- Type for StateT above ExceptT which above on some
-- "m" which is Monad and "r" is result of StateT and
-- others monads
type StateWE t m r = StateT t (ExceptT String m) r

-- Directory, start path and current work path storage
data WorkingEnvironment = WorkingEnvironment {
  weStartPath   :: FilePath,
  weCurrentPath :: [FilePath],
  weWorkAround  :: Directory
} deriving (Eq)

-- commented for debug
-- instance Show WorkingEnvironment where
  -- show WorkingEnvironment {..} = "Current working dir: \"" ++ intercalate "/" weCurrentPath ++ "\"\nHere workAround________" ++ show weWorkAround

instance Show WorkingEnvironment where
  show WorkingEnvironment {..} = printTree 0 "" weWorkAround

class WorkingEnvironmentable t where
  weCommandCD :: (Monad m) => FilePath -> StateWE t m ()
  weCommandInformation :: (Monad m) => FilePath -> StateWE t m String
  weCommandFindFile :: (Monad m) => String -> StateWE t m String
  weCommandCat :: (Monad m) => String -> StateWE t m String
  weCommandCreateFolder :: (Monad m) => String -> StateWE t m ()
  weCommandCreateFile :: (Monad m) => String -> UTCTime -> StateWE t m ()
  weCommandRemove :: (Monad m) => FilePath -> StateWE t m ()
  weCommandWriteFile :: (Monad m) => FilePath -> String -> UTCTime -> StateWE t m ()
  weCommandDir :: (Monad m) => StateWE t m String
  weCommandLS :: (Monad m) => FilePath -> StateWE t m String
  weCommandCVSInit :: (Monad m) => UTCTime -> StateWE t m ()
  weCommandCVSAdd :: (Monad m) => FilePath -> UTCTime -> StateWE t m ()
  weCommandCVSUpdate :: (Monad m) => FilePath -> String -> UTCTime -> StateWE t m ()
  weCommandCVSCat :: (Monad m) => FilePath -> String -> StateWE t m String
  weCommandCVSMergeRevs :: (Monad m) => FilePath -> String -> String -> String -> UTCTime -> StateWE t m ()
  weCommandCVSDeleteVersion :: (Monad m) => FilePath -> String -> StateWE t m ()
  weCommandCVSRemove :: (Monad m) => FilePath -> StateWE t m ()
  weCommandCVSShowEverything :: (Monad m) => StateWE t m String
  weCommandCVSHistory :: (Monad m) => FilePath -> StateWE t m String
  weGetCurrentDirectoryCMD :: t -> String
  weGetLocalPath :: t -> String

instance WorkingEnvironmentable WorkingEnvironment where
  weGetCurrentDirectoryCMD WorkingEnvironment {..} = weStartPath ++ (if (null weCurrentPath) then "" else "/" ++ (intercalate "/" weCurrentPath))
  weGetLocalPath WorkingEnvironment {..} = intercalate "/" weCurrentPath

  weCommandCD :: forall m. (Monad m) => FilePath -> StateWE WorkingEnvironment m ()
  weCommandCD dir = do
    WorkingEnvironment {..} <- get
    let pathList = splitOn "/" dir
    handleCases pathList
    return ()
    where
      handleCases :: [FilePath] -> StateWE WorkingEnvironment m ()
      handleCases pathList
        | onlyDots pathList = do
          WorkingEnvironment {..} <- get
          if (length pathList > length weCurrentPath)
          then
            lift $ throwE $ "Don't more than maximum length to root local FileSystem " ++ (show $ length pathList) ++ " > " ++ (show $ length weCurrentPath)
          else
            modify (\s -> s { weCurrentPath = take ((length weCurrentPath) - (length pathList)) weCurrentPath})
        | onlyNotDots pathList = do
          WorkingEnvironment {..} <- get
          let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
          let expectedWorkDirectory = weGetCurrentWorkAround (currentWorkDirectory) pathList
          case expectedWorkDirectory of
            Just _ -> modify (\s -> s { weCurrentPath = (weCurrentPath ++ pathList) })
            Nothing -> lift $ throwE $ "\"" ++ dir ++ "\"" ++ " does not exist"
        | otherwise  = do
          lift $ throwE $ "Don't use multypath with \"..\", only \"..\" or without \"..\" in: \"" ++ show pathList ++ "\""
      onlyDots :: [FilePath] -> Bool
      onlyDots list = (length (filter (\x -> x == "..") list )) == (length list)
      onlyNotDots :: [FilePath] -> Bool
      onlyNotDots list = (length (filter (\x -> x /= "..") list )) == (length list)

  weCommandInformation path = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isDirectory currentWorkDirectory pathList)
    then do
      let (Just dir) = weGetCurrentWorkAround currentWorkDirectory pathList
      return $ dGetInformation dir
    else
      if (fromMaybe False $ isFile currentWorkDirectory pathList)
      then do
        let (Just dir) = weGetCurrentWorkAround currentWorkDirectory (init pathList)
        let file = dGetFileByName dir (last pathList)
        return $ fGetInformation file
      else do
        if (((length pathList == 1) && (head pathList == "")) || ((length pathList == 1) && (head pathList == ".")))
        then do
          return $ dGetInformation currentWorkDirectory
        else lift $ throwE $ "Can't find file or directory \"" ++ (intercalate "/" pathList) ++ "\""

  weCommandFindFile name = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    case (dFindFileByName currentWorkDirectory name) of
      Just x -> return $ "\"" ++ fPath x ++ "\""
      Nothing -> lift $ throwE $ "File not found: \"" ++ name ++ "\""

  weCommandCat :: forall m. (Monad m) => String -> StateWE WorkingEnvironment m String
  weCommandCat name = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" name
    handleCases pathList currentWorkDirectory
    where
      handleCases :: [String] -> Directory -> StateWE WorkingEnvironment m String
      handleCases list currentWorkDirectory
        | ((length list == 1) && (head list == "")) = lift $ throwE $ "Not allow use empty name"
        | ((length list == 1) && (head list /= "")) = do
                let file = filter (\x -> (fGetName x) == name ) $ dGetFiles currentWorkDirectory
                if (null file)
                then
                  lift $ throwE $ "Can't find file with name: \"" ++ name ++ "\""
                else
                  return $ fContent $ head file
        | otherwise = do
          case (weGetCurrentWorkAround currentWorkDirectory $ init list) of
            Just currentWorkDirectory' -> do
              let file = filter (\x -> (fGetName x) == (last list) ) $ dGetFiles currentWorkDirectory'
              if (null file)
              then
                lift $ throwE $ "Can't find file with name: \"" ++ last list ++ "\""
              else
                return $ fContent $ head file
            Nothing -> lift $ throwE $ "Can't find directory with file: \"" ++ (intercalate "/" (init list)) ++ "\""

  weCommandCreateFolder name = do
    if ((length $ splitOn "/" name) == 1)
    then do
      WorkingEnvironment {..} <- get
      let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
      case (find (\x -> (dGetName x) == name) (dSubDirectories currentWorkDirectory)) of
        Just x -> lift $ throwE $ "Can't create folder, this name is taken \"" ++ (dGetPath x) ++ "\""
        Nothing -> do
          modify (\s -> s {weWorkAround = (recurseConstructDirectory weWorkAround weCurrentPath name)})
    else
      lift $ throwE $ "Can't create folder, this name contain relative path \"" ++ name ++ "\""

  weCommandCreateFile name currentTime = do
    if ((length $ splitOn "/" name) == 1)
    then do
      WorkingEnvironment {..} <- get
      let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
      case (find (\x -> (fGetName x) == name) (dFiles currentWorkDirectory)) of
        Just x -> lift $ throwE $ "Can't create file, this name is taken \"" ++ (fGetPath x) ++ "\""
        Nothing -> do
          modify (\s -> s {weWorkAround = (recurseConstructFile weWorkAround weCurrentPath name currentTime)})
    else
      lift $ throwE $ "Can't create file, this name contain relative path \"" ++ name ++ "\""

  weCommandRemove path = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isDirectory currentWorkDirectory pathList)
    then do
      let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
      modify (\s -> s {weWorkAround = recurseDeleteDirectory weWorkAround leftPart rightPart})
    else
      if (fromMaybe False $ isFile currentWorkDirectory pathList)
      then do
        let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
        modify (\s -> s {weWorkAround = recurseDeleteFile weWorkAround leftPart rightPart})
      else lift $ throwE $ "Can't find file or directory for removing: \"" ++ (show pathList) ++ "\""

  weCommandWriteFile path text time = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isFile currentWorkDirectory pathList)
    then do
      let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
      modify (\s -> s {weWorkAround = recurseEditFileContext weWorkAround leftPart rightPart text time})
    else lift $ throwE $ "Can't find file \"" ++ (intercalate "/" pathList) ++ "\""

  weCommandDir = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    return $ intercalate "\n" $ (map dGetName $ dSubDirectories currentWorkDirectory) ++ (map fGetName $ dFiles currentWorkDirectory)

  weCommandLS path = do
    WorkingEnvironment {..} <- get
    let pathList = splitOn "/" path
    case (weGetCurrentWorkAround weWorkAround (weCurrentPath ++ pathList)) of
      Just needDirectory -> do
        return $ intercalate "\n" $ (map dGetName $ dSubDirectories needDirectory) ++ (map fGetName $ dFiles needDirectory)
      Nothing ->
        lift $ throwE $ "\"" ++ (intercalate "/" pathList) ++ "\" does not exist"

  weCommandCVSInit time = do
    WorkingEnvironment {..} <- get
    if (null weCurrentPath)
    then
      modify (\s -> s {weWorkAround = dCVSAddDirectory weWorkAround time})
    else do
      let (leftPart, rightPart) = correctList (weCurrentPath)
      modify (\s -> s {weWorkAround = recurseAddCVSDirectory weWorkAround leftPart rightPart time})

  weCommandCVSAdd path time = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isDirectory currentWorkDirectory pathList)
    then do
      let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
      modify (\s -> s {weWorkAround = recurseAddCVSDirectory weWorkAround leftPart rightPart time})
    else
      if (fromMaybe False $ isFile currentWorkDirectory pathList)
      then do
        let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
        modify (\s -> s {weWorkAround = recurseAddCVSFile weWorkAround leftPart rightPart time})
      else lift $ throwE $ "Can't find file or directory for adding in CVS: \"" ++ (intercalate "/" pathList) ++ "\""

  weCommandCVSUpdate path text time = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isFile currentWorkDirectory pathList)
    then do
      let (Just needDir) = (weGetCurrentWorkAround weWorkAround (weCurrentPath ++ (init pathList)))
      case (dFindCVSDirSub needDir (takeFileName path)) of
        Just _ -> do
          let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
          modify (\s -> s {weWorkAround = recurseCVSUpdate weWorkAround leftPart rightPart text time})
        Nothing -> lift $ throwE $ "File \"" ++ (intercalate "/" pathList) ++ "\" is untracked"
    else lift $ throwE $ "Can't find file \"" ++ (intercalate "/" pathList) ++ "\""

  weCommandCVSCat path version = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isFile currentWorkDirectory pathList)
    then do
      let (Just needDir) = (weGetCurrentWorkAround weWorkAround (weCurrentPath ++ (init pathList)))
      case (dFindCVSDirSub needDir (takeFileName path)) of
        Just needCVSDir -> do
          let needDirVersion = filter (\x -> (dGetName x) == version) (dGetSubDirectories needCVSDir)
          if (null needDirVersion)
          then lift $ throwE $ "Commit \"" ++ version ++ "\" not found"
          else do
            let needDirWithFile = head needDirVersion
            let File{..} = head $ filter (\x -> (fGetName x) == (takeFileName path)) (dGetFiles needDirWithFile)
            return $ fContent
        Nothing -> lift $ throwE $ "File \"" ++ (intercalate "/" pathList) ++ "\" is untracked"
    else lift $ throwE $ "Can't find file \"" ++ (intercalate "/" pathList) ++ "\""

  weCommandCVSMergeRevs path version1 version2 strategy time = do
    fileContent1 <- weCommandCVSCat path version1
    fileContent2 <- weCommandCVSCat path version2
    case strategy of
      "left" -> do
        weCommandWriteFile path fileContent1 time
        weCommandCVSUpdate path ("Merged commits \"" ++ version1 ++ "\" and \"" ++ version2 ++ "\", was choosen " ++ version1) time
      "right" -> do
        weCommandWriteFile path fileContent2 time
        weCommandCVSUpdate path ("Merged commits \"" ++ version1 ++ "\" and \"" ++ version2 ++ "\", was choosen " ++ version2) time
      "both" -> do
        weCommandWriteFile path (fileContent1 ++ "\n>>>>\n" ++ fileContent2) time
        weCommandCVSUpdate path ("Merged commits \"" ++ version1 ++ "\" and \"" ++ version2 ++ "\", was choosen both") time
      _ -> return ()
  weCommandCVSDeleteVersion path version = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isFile currentWorkDirectory pathList)
    then do
      let (Just needDir) = (weGetCurrentWorkAround weWorkAround (weCurrentPath ++ (init pathList)))
      case (dFindCVSDirSub needDir (takeFileName path)) of
        Just _ -> do
          let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
          modify (\s -> s {weWorkAround = recurseCVSDeleteVersion weWorkAround leftPart rightPart version})
        Nothing -> lift $ throwE $ "File \"" ++ (intercalate "/" pathList) ++ "\" is untracked"
    else lift $ throwE $ "Can't find file \"" ++ (intercalate "/" pathList) ++ "\""

  weCommandCVSRemove path = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isFile currentWorkDirectory pathList)
    then do
      let (Just needDir) = (weGetCurrentWorkAround weWorkAround (weCurrentPath ++ (init pathList)))
      case (dFindCVSDirSub needDir (takeFileName path)) of
        Just _ -> do
          let (leftPart, rightPart) = correctList (weCurrentPath ++ pathList)
          modify (\s -> s {weWorkAround = recurseCVSRemove weWorkAround leftPart rightPart})
        Nothing -> lift $ throwE $ "File \"" ++ (intercalate "/" pathList) ++ "\" is untracked yet"
    else lift $ throwE $ "Can't find file \"" ++ (intercalate "/" pathList) ++ "\""

  weCommandCVSShowEverything = do
    we@(WorkingEnvironment {..}) <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let prefix = weGetCurrentDirectoryCMD we ++ "/"
    let filePaths = map (\path -> drop (length prefix) path) (dGetAllFilesPaths currentWorkDirectory)
    return $ concatMap (\path -> do
      Right info <- runExceptT $ runStateT (weCommandCVSHistory path) we
      "\"" ++ path ++ "\"\n" ++ (fst info) ++ "\n") filePaths

  weCommandCVSHistory path = do
    WorkingEnvironment {..} <- get
    let (Just currentWorkDirectory) = weGetCurrentWorkAround weWorkAround weCurrentPath
    let pathList = splitOn "/" path
    if (fromMaybe False $ isFile currentWorkDirectory pathList)
    then do
      let (Just needDir) = (weGetCurrentWorkAround weWorkAround (weCurrentPath ++ (init pathList)))
      case (dFindCVSDirSub needDir (takeFileName path)) of
        Just cvsDir -> do
          let infoEach' = map (\dir -> ((dGetName dir), (fGetContents $ head $ filter (\x -> (fGetName x) == "commit") (dGetFiles dir)))) (dGetSubDirectories cvsDir)
          let sorted = sortBy (\(x, _) (y, _) -> compare (read x :: Int) (read y :: Int)) infoEach'
          let infoEach = map (\(x,y) -> x ++ ". " ++ y) sorted
          return $ intercalate "\n" infoEach
        Nothing -> lift $ throwE $ "File \"" ++ (intercalate "/" pathList) ++ "\" is untracked"
    else lift $ throwE $ "Can't find file \"" ++ (intercalate "/" pathList) ++ "\""

weGetCurrentWorkAround :: Directory -> [FilePath] -> Maybe Directory
weGetCurrentWorkAround startDir path = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Maybe Directory
    recurseGo Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      if (null sort')
      then
        Nothing
      else
        recurseGo (head sort') xs
    recurseGo dir [] = Just dir

recurseConstructDirectory :: Directory -> [FilePath] -> String -> Directory
recurseConstructDirectory startDir path name = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dAddSubDirectory dir name

recurseDeleteDirectory :: Directory -> [FilePath] -> String -> Directory
recurseDeleteDirectory startDir path name = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dDeleteSubDirectory dir name

recurseAddCVSDirectory :: Directory -> [FilePath] -> String -> UTCTime -> Directory
recurseAddCVSDirectory startDir path name time = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo d@Directory{..} [] = do
      let sort' = filter (\p -> name == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> name /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [dCVSAddDirectory (head sort') time]}

recurseConstructFile :: Directory -> [FilePath] -> String -> UTCTime -> Directory
recurseConstructFile startDir path name time = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dAddFile dir name time

recurseDeleteFile :: Directory -> [FilePath] -> String -> Directory
recurseDeleteFile startDir path name = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dDeleteFile dir name

recurseAddCVSFile :: Directory -> [FilePath] -> String -> UTCTime -> Directory
recurseAddCVSFile startDir path name time = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dCVSAddFile dir name time

recurseEditFileContext :: Directory -> [FilePath] -> String -> String -> UTCTime -> Directory
recurseEditFileContext startDir path name text time = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dEditFileContext dir name text time

recurseCVSUpdate :: Directory -> [FilePath] -> String -> String -> UTCTime -> Directory
recurseCVSUpdate startDir path name text time = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dCVSUpdate dir name text time

recurseCVSDeleteVersion :: Directory -> [FilePath] -> String -> String -> Directory
recurseCVSDeleteVersion startDir path name version = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dCVSDeleteVersion dir name version

recurseCVSRemove :: Directory -> [FilePath] -> String  -> Directory
recurseCVSRemove startDir path name = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Directory
    recurseGo d@Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      let withoutNeedDirectory = filter (\p -> x /= (dGetName p)) dSubDirectories
      d {dSubDirectories = withoutNeedDirectory ++ [(recurseGo (head sort') xs)]}
    recurseGo dir [] = dCVSRemove dir name

isDirectory :: Directory -> [FilePath] -> Maybe Bool
isDirectory startDir path = recurseGo startDir path
  where
    recurseGo ::  Directory -> [FilePath] -> Maybe Bool
    recurseGo Directory{..} (x:xs) = do
      let sort' = filter (\p -> x == (dGetName p)) dSubDirectories
      if (null sort')
      then
        Just False
      else
        recurseGo (head sort') xs
    recurseGo _ [] = Just True

isFile :: Directory -> [FilePath] -> Maybe Bool
isFile startDir path = do
    let endPath = last path
    case (weGetCurrentWorkAround startDir (init path)) of
      Just (Directory {..}) -> do
        if (length (filter (\f -> (fGetName f) == endPath) dFiles) > 0)
        then
          Just True
        else
          Just False
      Nothing -> Just False

correctList :: [String] -> ([String], String)
correctList list
  | (length list == 1) = ([], head list)
  | (length list >= 2) = (init list, last list)
  | otherwise = ([], "")
