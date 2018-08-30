{-# LANGUAGE NamedFieldPuns #-}

module Data.Config (
  BlogConfig(BlogConfig), 
  cfg_port, 
  cfg_dbFile, 
  cfg_maybeContentDir, 
  loadConfig
) where

import qualified Database.SQLite.Simple  as SQLite
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data BlogConfig = BlogConfig { 
  cfg_port :: Int, 
  cfg_dbFile :: FilePath, 
  cfg_maybeContentDir :: Maybe FilePath  
}

defaultPort :: Int
defaultPort = 8008

lookupPortInEnv :: IO Int
lookupPortInEnv = do
  maybePortString <- lookupEnv "PORT"
  case maybePortString of
    Just portString -> 
      case readMaybe portString of
        Just portInt -> return portInt
        Nothing -> error $ "PORT environment variable expected an integer but instead got \"" ++ portString ++ "\"."
    Nothing -> do
      putStrLn $ "PORT environment variable not set. Defaulting to " ++ (show defaultPort)
      return defaultPort

lookupSqliteFileInEnv :: IO FilePath
lookupSqliteFileInEnv = do 
  maybeFilePath <- lookupEnv "SQLITE_FILE"
  case maybeFilePath of
    Just filePath -> return filePath
    Nothing -> error "SQLITE_FILE environment variable not set. You must provide a valid file path to open the SQLite database."

lookupContentDirInEnv :: IO (Maybe FilePath)
lookupContentDirInEnv = do 
  maybeFilePath <- lookupEnv "CONTENT_DIR"
  case maybeFilePath of
    Just filePath -> do
       fileExists <- doesFileExist $ filePath </> "update.sh"
       if fileExists then return $ Just filePath else do 
         putStrLn $ "\"update.sh\" not found in directory \"" ++ filePath ++ 
           "\". CI web hook is disabled unless you provide a valid path to the directory that contains an \"update.sh\" script to run on every invocation."
         return Nothing

    Nothing -> do 
      putStrLn "CONTENT_DIR environment variable not set. CI web hook is disabled unless you provide a valid path to the directory that contains an \"update.sh\" script to run on every invocation."
      return Nothing

loadConfig :: IO BlogConfig
loadConfig = BlogConfig <$> lookupPortInEnv <*> lookupSqliteFileInEnv <*> lookupContentDirInEnv
