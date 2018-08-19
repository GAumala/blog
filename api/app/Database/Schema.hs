{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module Database.Schema (
  BlogDB(_blogLikes, _blogPosts, _blogReaders),
  Like, 
  LikeT(Like, _likeReaderKey, _likePostKey),
  Post, 
  PostT(Post, _postStringId),
  Reader, 
  ReaderT(Reader, _readerId, _readerIpAddress, _readerUserAgent),
  blogDb) where

import qualified Data.Text.Lazy as Text
import Database.Beam

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


