{-# LANGUAGE FlexibleInstances #-}

module Web.DBActions (DBClient, incrementLikesCount, getLikesCount) where 

import Data.Text.Lazy (Text)
import Database.SQLite.Simple (Connection)
import Web.Scotty (liftAndCatchIO, ActionM)

import qualified Database.Queries as Q
import Data.Models (
  LikeInfo (LikeInfo, readerInfo, postStringId))

class DBClient m where
  incrementLikesCount :: Connection -> LikeInfo -> m (Maybe Int)
  getLikesCount :: Connection -> Text -> m (Maybe Int)

instance DBClient ActionM where
  incrementLikesCount conn likeInfo = 
    liftAndCatchIO $ Q.incrementLikesCount conn likeInfo
  getLikesCount conn postStringId = 
    liftAndCatchIO $ Q.getLikesCount conn postStringId
