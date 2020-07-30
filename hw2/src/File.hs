{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module File where

import Data.Maybe
import qualified Data.Text as DT
import Data.Time.Clock
import System.Directory
import System.FilePath.Posix

-- Contain file status storage
data File = File {
   fPath        :: FilePath
  ,fContent     :: String
  ,fPermissions :: Permissions
  ,fEditTime    :: UTCTime
  ,fEdited      :: Bool
} deriving (Eq)

instance Show File where
  show = fGetName

class Fileble f where
  fGetName         :: f -> String
  fIsEdited        :: f -> Bool
  fGetContents     :: f -> String
  fGetPath         :: f -> String
  fGetInformation  :: f -> String
  fGetSize         :: f -> Int
  fGetEditTime     :: f -> UTCTime
  fGetPermissions  :: f -> Permissions
  fGetType         :: f -> String
  fShowPermissions :: f -> String
  fSaveFile        :: f -> IO ()

instance Fileble File where
  fGetName = takeFileName . fPath
  fIsEdited = fEdited
  fGetContents = fContent
  fGetPath = fPath
  fGetInformation = (fromMaybe "Something was bad duo to count information") . showFileInformation
  fGetSize = DT.length . DT.pack . fGetContents
  fGetEditTime = fEditTime
  fGetPermissions = fPermissions
  fGetType = takeExtension . fGetPath
  fShowPermissions f = (fromMaybe "Something was bad duo to gets permissons") $ do
    let perm = fGetPermissions f
    Just ("\"" ++ isReadable perm ++ isWritable perm ++ isExecutable perm ++ isSearchable perm ++ "\"")
    where
      isReadable perm = if (readable perm) then "r" else ""
      isWritable perm = if (writable perm) then "w" else ""
      isExecutable perm = if (executable perm) then "e" else ""
      isSearchable perm = if (searchable perm) then "s" else ""
  fSaveFile File{..} = do
      _ <- writeFile fPath fContent
      _ <- setModificationTime fPath fEditTime
      return ()

readFile' :: FilePath -> IO File
readFile' currentPath = do
  fileContent <- readFile currentPath
  permission <- getPermissions currentPath
  time <- getModificationTime currentPath
  return $ File currentPath fileContent permission time False

showFileInformation :: File -> Maybe String
showFileInformation file = do
  let path = fGetPath file
  let permissions = fShowPermissions file
  let extension = takeExtension path
  let date = fGetEditTime file
  let size = fGetSize file
  return ("Path: \"" ++ path ++
   "\"\nPermissions: " ++ permissions ++
   "\nExtension: \"" ++ extension ++
   "\"\nLast updated: " ++ show date ++
   "\nSize: " ++ show size ++ " bytes\n")
