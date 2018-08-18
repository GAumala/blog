
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE DuplicateRecordFields #-}

module DB (incrementLikesCount) where

import Database.Beam
import Database.Beam.Sqlite
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExt
import Database.SQLite.Simple (open, Connection)
import qualified Data.Text as Text

-- specify types for the tables
data PostT f = Post
  { _postId :: Columnar f Int
  , _postStringId :: Columnar f Text.Text
  } deriving (Generic)

data ReaderT f = Reader
  { _readerId :: Columnar f Int
  , _readerIpAddress :: Columnar f Text.Text
  , _readerUserAgent :: Columnar f Text.Text
  } deriving (Generic)

data LikeT f = Like
  { _likeId :: Columnar f Int
  , _likeReaderKey :: PrimaryKey ReaderT f
  , _likePostKey :: PrimaryKey PostT f
  } deriving (Generic)
--
-- table types that I can actually use
type Post = PostT Identity
type Reader = ReaderT Identity
type Like = LikeT Identity


-- derive instances (Beam boilerplate)
deriving instance Show Post
deriving instance Show (PrimaryKey PostT Identity)
instance Beamable PostT
instance Beamable (PrimaryKey PostT)

deriving instance Show Reader
deriving instance Show (PrimaryKey ReaderT Identity)
instance Beamable ReaderT
instance Beamable (PrimaryKey ReaderT)

deriving instance Show Like
instance Beamable LikeT
instance Beamable (PrimaryKey LikeT)

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f Int) deriving Generic
  primaryKey = PostId . (_postId :: PostT f -> Columnar f Int)

instance Table ReaderT where
  data PrimaryKey ReaderT f = ReaderId (Columnar f Int) deriving Generic
  primaryKey = ReaderId . (_readerId :: ReaderT f -> Columnar f Int)

instance Table LikeT where
  data PrimaryKey LikeT f = LikeId (Columnar f Int) deriving Generic
  primaryKey = LikeId . (_likeId :: LikeT f -> Columnar f Int)

-- types for the database
data BlogDB f = BlogDB
  { _blogPosts :: f (TableEntity PostT)
  , _blogReaders :: f (TableEntity ReaderT)
  , _blogLikes :: f (TableEntity LikeT)
  } deriving (Generic)

instance Database be BlogDB

blogDb :: DatabaseSettings be BlogDB
blogDb = defaultDbSettings

findPostByStringId :: Text.Text -> SqliteM (Maybe Post)
findPostByStringId targetStringId = 
  runSelectReturningOne 
    $ select
    $ filter_ (\row -> _postStringId row ==. val_ targetStringId)
    $ all_ (_blogPosts blogDb)

findReader :: Text.Text -> Text.Text -> SqliteM (Maybe Reader)
findReader ipAddress userAgent = 
  runSelectReturningOne 
    $ select
    $ filter_ (\row -> _readerIpAddress row ==. val_ ipAddress 
                   &&. _readerUserAgent row ==. val_ userAgent)
    $ all_ (_blogReaders blogDb)

insertNewPost :: Text.Text -> SqliteM (PrimaryKey PostT Identity)
insertNewPost stringId = do
  [newPost] <- BeamExt.runInsertReturningList (_blogPosts blogDb) 
                 $ insertExpressions [ Post default_ (val_ stringId) ]
  return $ pk newPost

insertNewReader :: Text.Text -> Text.Text -> SqliteM (PrimaryKey ReaderT Identity)
insertNewReader ipAddress userAgent = do
  [newReader] <- BeamExt.runInsertReturningList (_blogReaders blogDb) 
                 $ insertExpressions [ Reader default_ (val_ ipAddress) (val_ userAgent)]
  return $ pk newReader

insertNewLike :: (PrimaryKey ReaderT Identity) -> (PrimaryKey PostT Identity) -> SqliteM ()
insertNewLike readerKey postKey = runInsert
  $ insert (_blogLikes blogDb)
  $ insertExpressions [ Like default_ (val_ readerKey) (val_ postKey) ]
  
getPostKey :: Text.Text -> SqliteM (PrimaryKey PostT Identity)
getPostKey stringId = do
  maybePost <- findPostByStringId stringId
  case maybePost of
    Just post -> return $ pk post
    Nothing -> insertNewPost stringId

getReaderKey :: Text.Text -> Text.Text -> SqliteM (PrimaryKey ReaderT Identity)
getReaderKey ipAddress userAgent = do
  maybeReader <- findReader ipAddress userAgent
  case maybeReader of
    Just reader -> return $ pk reader
    Nothing -> insertNewReader ipAddress userAgent

incrementLikesCount :: Connection -> Text.Text -> Text.Text -> Text.Text -> IO ()
incrementLikesCount conn ipAddress userAgent postStringId = runBeamSqliteDebug putStrLn conn $ do
  postKey <- getPostKey postStringId
  readerKey <- getReaderKey ipAddress userAgent
  insertNewLike readerKey postKey
  
