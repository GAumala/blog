{-# LANGUAGE OverloadedStrings #-}

module Web.Actions (getLikes, postLike) where

import Network.HTTP.Types.Status (ok200)
import qualified Database.SQLite.Simple  as SQLite
import Web.Scotty (json, liftAndCatchIO, param, status, text, ActionM)

import Data.Models (
  IPAddress (IPAddress),
  ReaderInfo (ReaderInfo, ipAddress, userAgent),
  LikeInfo (LikeInfo, readerInfo, postStringId),
  UserAgent (UserAgent))
import Web.Scotty.Helpers (
  APIError (httpStatus, msg), 
  getIpAddress, 
  getUserAgent,
  whenValid) 
import Web.DBActions (incrementLikesCount)

getNewLikeInfoFromRequest :: ActionM (Either APIError LikeInfo)
getNewLikeInfoFromRequest = do
  ipAddress <- getIpAddress
  userAgent <- getUserAgent
  postStringId <- param "stringId"
  let readerInfo = ReaderInfo <$> ipAddress <*> userAgent
  return $ LikeInfo postStringId <$> readerInfo

postLike :: SQLite.Connection -> ActionM ()
postLike conn = do
  likeInfo <- getNewLikeInfoFromRequest
  whenValid likeInfo $ \validLikeInfo -> do
      incrementLikesCount conn validLikeInfo
      status ok200

getLikes :: ActionM ()
getLikes = text "0"

