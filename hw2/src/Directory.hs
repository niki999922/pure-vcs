{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Directory where

import File

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Clock
import System.Directory
import System.FilePath.Posix

-- Contain directory status storage
data Directory = Directory {
   dPath              :: FilePath
  ,dFiles             :: [File]
  ,dSubDirectories    :: [Directory]
  ,dPermissions       :: Permissions
  ,dVCSSubDirectories :: [Directory]
} deriving (Eq)

class Directorible d where
  dGetName               :: d -> String
  dGetPath               :: d -> FilePath
  dGetFiles              :: d -> [File]
  dGetAmountFiles        :: d -> Int
  dGetPermissions        :: d -> Permissions
  dShowPermissions       :: d -> String
  dGetSubDirectories     :: d -> [Directory]
  dGetVCSSubDirectories  :: d -> [Directory]
  dGetInformation        :: d -> String
  dGetSizeDirectory      :: d -> Int
  dGetFileByName         :: d -> String -> File
  dFindFileByName        :: d -> String -> Maybe File
  dAddSubDirectory       :: d -> String -> d
  dAddFile               :: d -> String -> UTCTime -> d
  dDeleteFile            :: d -> String -> d
  dDeleteSubDirectory    :: d -> String -> d
  dEditFileContext       :: d -> String -> String -> UTCTime -> d
  dSaveDirectory         :: d -> IO ()
  dCVSAddFile            :: d -> String -> UTCTime -> d
  dCVSAddDirectory       :: d -> UTCTime -> d
  dCreateCVSDir          :: d -> String -> UTCTime -> d
  dCVSUpdate             :: d -> String -> String -> UTCTime -> d
  dFindCVSDirSub         :: d -> String -> Maybe d
  dCVSLastNumberOfCommit :: d -> Int
  dCVSDeleteVersion      :: d -> String -> String -> d
  dCVSRemove             :: d -> String -> d
  dGetAllFilesPaths      :: d -> [FilePath]

instance Show Directory where
  show d = "\n\n" ++ (dPath d) ++ ":\n" ++ (show $ dFiles d) ++ "\nVCS_SubDir: " ++ (show $ dVCSSubDirectories d) ++ "\nSubDir: " ++ (show $ dSubDirectories d)

instance Directorible Directory where
  dGetName = takeFileName . dPath
  dGetPath = dPath
  dGetFiles = dFiles
  dGetAmountFiles = length . dGetFiles
  dGetSubDirectories = dSubDirectories
  dGetVCSSubDirectories = dVCSSubDirectories
  dGetInformation = (fromMaybe "Something was bad duo to count information") . showDirectoryInformation
  dGetPermissions = dPermissions
  dGetSizeDirectory d = (sum (map (dGetSizeDirectory) (dGetSubDirectories d))) + (sum (map (fGetSize) (dGetFiles d)))
  dShowPermissions d = fromMaybe "Something was bad duo to gets permissons" $ do
    let perm = dGetPermissions d
    Just ("\"" ++ isReadable perm ++ isWritable perm ++ isExecutable perm ++ isSearchable perm ++ "\"")
    where
      isReadable perm = if (readable perm) then "r" else ""
      isWritable perm = if (writable perm) then "w" else ""
      isExecutable perm = if (executable perm) then "e" else ""
      isSearchable perm = if (searchable perm) then "s" else ""
  dGetFileByName d name = head $ filter (\x -> (fGetName x) == name) (dGetFiles d)
  dFindFileByName Directory{..} name = do
    let files = filter (\x -> (fGetName x) == name) dFiles
    if (null files)
    then do
      let res = filter isJust $ map (`dFindFileByName` name) dSubDirectories
      if (null res)
      then
        Nothing
      else
        head res
    else
      Just $ head files
  dAddSubDirectory d@Directory{..} name = d {dSubDirectories = dSubDirectories ++ [Directory (dPath ++ "/" ++ name) [] [] allPermission []]}
  dAddFile d@Directory{..} name time = d {dFiles = dFiles ++ [File (dPath ++ "/" ++ name) "" allPermission time True]}
  dDeleteFile d@Directory{..} name = d {dFiles = filter (\x -> (fGetName x) /= name) dFiles}
  dDeleteSubDirectory d@Directory{..} name = d {dSubDirectories = filter (\x -> (dGetName x) /= name) dSubDirectories}
  dEditFileContext d@Directory{..} name text time = do
    let otherFiles = filter (\x -> (fGetName x) /= name) dFiles
    let editedFile = head $ filter (\x -> (fGetName x) == name) dFiles
    d {dFiles = otherFiles ++ [editedFile { fContent = text, fEditTime = time, fEdited = True}]}
  dSaveDirectory Directory{..} = do
    mapM_ fSaveFile dFiles
    mapM_ (\x -> createDirectory $ dGetPath x) dSubDirectories
    mapM_ (\x -> createDirectory $ dGetPath x) dVCSSubDirectories
    mapM_ (dSaveDirectory) dSubDirectories
    mapM_ (dSaveDirectory) dVCSSubDirectories
  dCVSAddFile d@Directory{..} name time = do
    let expectedDir = filter (\x -> (dGetName x) == (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
    if (null expectedDir)
    then
      dCreateCVSDir d name time
    else
      d
  dCreateCVSDir d@Directory{..} name time = do
    let needFile =  head $ filter (\x -> (fGetName x) == name) dFiles
    let copyFile' = needFile {fPath = (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/0/" ++ name) }
    let commitFile = File (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/0/commit") "initial" allPermission time True
    let subCVSDir = Directory (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/0") [copyFile', commitFile] [] allPermission []
    let newCVSDir = Directory (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name))) [] [subCVSDir] allPermission []
    d {dVCSSubDirectories =  dVCSSubDirectories ++ [newCVSDir]}
  dCVSAddDirectory d@Directory{..} time = do
    let filesNames = map fGetName dFiles
    let newDirState = foldl' (\x y -> dCVSAddFile x y time) d filesNames
    let newSubDirState = map (`dCVSAddDirectory` time) $ dGetSubDirectories newDirState
    newDirState {dSubDirectories = newSubDirState}
  dCVSUpdate d@Directory{..} name text time = do
    let otherCvs = filter (\x -> (dGetName x) /= (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
    let (Just cvsDir) = dFindCVSDirSub d name
    let maxNumber = dCVSLastNumberOfCommit cvsDir
    let needFile =  head $ filter (\x -> (fGetName x) == name) dFiles
    let copyFile' = needFile {fPath = (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/" ++ (show $ maxNumber + 1) ++ "/" ++ name) }
    let commitFile = File (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/" ++ (show $ maxNumber + 1) ++ "/commit") text allPermission time True
    let subCVSDir = Directory (dPath ++ "/" ++ (".VCS_" ++ (takeFileName name)) ++ "/" ++ (show $ maxNumber + 1)) [copyFile', commitFile] [] allPermission []
    let updatedCVS = cvsDir { dSubDirectories = (dGetSubDirectories cvsDir) ++ [subCVSDir]}
    d {dVCSSubDirectories = otherCvs ++ [updatedCVS]}
  dCVSDeleteVersion d@Directory{..} name version = do
    let otherCvs = filter (\x -> (dGetName x) /= (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
    let (Just cvsDir) = dFindCVSDirSub d name
    let newSubDirectoriesCVS = filter (\x -> (dGetName x) /= version) (dGetSubDirectories cvsDir)
    let updatedCVS = cvsDir { dSubDirectories = newSubDirectoriesCVS}
    d {dVCSSubDirectories = otherCvs ++ [updatedCVS]}
  dCVSRemove d@Directory{..} name = do
    let otherCvs = filter (\x -> (dGetName x) /= (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
    d {dVCSSubDirectories = otherCvs}
  dFindCVSDirSub Directory{..} name = do
    let expectedDir = filter (\x -> (dGetName x) == (".VCS_" ++ (takeFileName name))) dVCSSubDirectories
    if (null expectedDir)
    then
      Nothing
    else
      Just $ head expectedDir
  dCVSLastNumberOfCommit Directory{..} = maximum $ map (\x -> read (dGetName x) :: Int) dSubDirectories
  dGetAllFilesPaths Directory{..} = (map fGetPath dFiles) ++ (concatMap dGetAllFilesPaths dSubDirectories)

readDirectoriesState :: FilePath -> IO Directory
readDirectoriesState currentPath = do
  elements' <- listDirectory currentPath
  let elements = map (\x -> currentPath ++ "/" ++ x) elements'
  files <- filterM doesFileExist elements
  directories <- filterM doesDirectoryExist elements
  convertedFiles <- mapM (readFile') files
  let cvsDirs = filter (\x -> "." `isPrefixOf`(takeFileName x)) directories
  let justDirs = filter (\x -> not ("." `isPrefixOf`(takeFileName x))) directories
  convertedJustDirectories <- mapM (readDirectoriesState) justDirs
  convertedCVSDirectories <- mapM (readDirectoriesState) cvsDirs
  permission <- getPermissions currentPath
  return $ Directory currentPath convertedFiles convertedJustDirectories permission convertedCVSDirectories

showDirectoryInformation :: Directory -> Maybe String
showDirectoryInformation dir = do
  let path = dGetPath dir
  let permissions = dShowPermissions dir
  let sizeDirectory = dGetSizeDirectory dir
  let amountFilesInDirectory = dGetAmountFiles dir
  return ("Path: \"" ++ path ++
   "\"\nPermissions: " ++ permissions ++
   "\nSize: " ++ show sizeDirectory ++ " bytes"++
   "\nFiles in directory: " ++ show amountFilesInDirectory ++ " files\n")

allPermission :: Permissions
allPermission = (setOwnerSearchable True (setOwnerExecutable True (setOwnerWritable True (setOwnerReadable True emptyPermissions))))

printTree :: Int -> String -> Directory -> String
printTree depth nameDir Directory {..} = (intercalate "\n" $ map (\x -> generateTabs depth nameDir ++ show x) $ map (takeFileName . fPath) dFiles) ++ showSubDirectories ++ showVCSSubDirectories
  where
    generateTabs :: Int -> String -> String
    generateTabs n nameDir' = (concat $ replicate n "|---") ++ nameDir' ++ "> "
    showVCSSubDirectories = do
      if (not $ null dVCSSubDirectories)
      then
        "\n" ++ (intercalate "\n" $ map (\x -> printTree (depth + 1) ("v_" ++ dGetName x) x) dVCSSubDirectories)
      else ""
    showSubDirectories = do
      if (not $ null dSubDirectories)
      then
        "\n" ++(intercalate "\n" $ map (\x -> printTree (depth + 1) ("s_" ++dGetName x) x) dSubDirectories)
      else ""
