{-# LANGUAGE FlexibleInstances #-}

module Web.DBActions (DBClient, incrementLikesCount) where 

import Database.SQLite.Simple (Connection)
import Web.Scotty (liftAndCatchIO, ActionM)

import qualified Database.Queries as Q
import Data.Models (
  LikeInfo (LikeInfo, readerInfo, postStringId))

class DBClient m where
  incrementLikesCount :: Connection -> LikeInfo -> m ()

instance DBClient ActionM where
  incrementLikesCount conn likeInfo = 
    liftAndCatchIO $ Q.incrementLikesCount conn likeInfo
