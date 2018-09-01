{-# LANGUAGE OverloadedStrings #-}

module Web.Actions (errorHandler, getLikes, postLike, updateStaticContent) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.IO (putStrLn)
import Network.HTTP.Types.Status (ok200, internalServerError500)
import qualified Database.SQLite.Simple  as SQLite
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Scripts (runUpdateScriptAtDir)
import Web.Scotty (param, status, text, ActionM)

import Data.Models (
  ReaderInfo (ReaderInfo),
  LikeInfo (LikeInfo))
import Web.Scotty.Helpers (
  APIError,
  getIpAddress, 
  getUserAgent,
  whenValid) 
import Web.DBActions (incrementLikesCount, getLikesCount)

getNewLikeInfoFromRequest :: ActionM (Either APIError LikeInfo)
getNewLikeInfoFromRequest = do
  ipAddress <- getIpAddress
  userAgent <- getUserAgent
  postStringId <- param "stringId"
  let readerInfo = ReaderInfo <$> ipAddress <*> userAgent
  return $ LikeInfo postStringId <$> readerInfo

respondWithLikesCount :: Int -> ActionM ()
respondWithLikesCount likesCount = text $ pack $ show likesCount

postLike :: SQLite.Connection -> ActionM ()
postLike conn = do
  likeInfo <- getNewLikeInfoFromRequest
  whenValid likeInfo $ \validLikeInfo -> do
    Just likesCount <- incrementLikesCount conn validLikeInfo
    respondWithLikesCount likesCount

getLikes :: SQLite.Connection -> ActionM ()
getLikes conn = do
  postStringId <- param "stringId"
  Just likesCount <- getLikesCount conn postStringId
  respondWithLikesCount likesCount

updateStaticContent :: FilePath -> ActionM ()
updateStaticContent staticContentDir = do
  exitCode <- liftIO $ runUpdateScriptAtDir staticContentDir
  case exitCode of
    ExitSuccess -> status ok200
    ExitFailure code -> do
      let msg = pack $ "Script exited with code " ++ (show code)
      status internalServerError500
      text msg 


errorHandler :: Text -> ActionM ()
errorHandler errorMessage = liftIO $ Data.Text.Lazy.IO.putStrLn errorMessage
