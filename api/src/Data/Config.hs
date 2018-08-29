{-# LANGUAGE NamedFieldPuns #-}

module Data.Config (
  BlogConfig(BlogConfig), 
  cfg_port, 
  cfg_dbFile, 
  cfg_maybeCIScript, 
  loadConfig
) where

import qualified Database.SQLite.Simple  as SQLite
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data BlogConfig = BlogConfig { 
  cfg_port :: Int, 
  cfg_dbFile :: FilePath, 
  cfg_maybeCIScript :: Maybe FilePath  
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

lookupCIScriptInEnv :: IO (Maybe FilePath)
lookupCIScriptInEnv = do 
  maybeFilePath <- lookupEnv "UPDATE_SCRIPT"
  case maybeFilePath of
    Just filePath -> return $ Just filePath
    Nothing -> do 
      putStrLn "UPDATE_SCRIPT environment variable not set. CI web hook is disabled unless you provide a valid file path with the script to run when it is triggered."
      return Nothing

loadConfig :: IO BlogConfig
loadConfig = BlogConfig <$> lookupPortInEnv <*> lookupSqliteFileInEnv <*> lookupCIScriptInEnv
