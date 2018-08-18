{-# LANGUAGE NamedFieldPuns #-}

module Database.Queries (incrementLikesCount) where

import Database.Beam
import Database.Beam.Sqlite
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExt
import Database.SQLite.Simple (Connection, withTransaction)
import qualified Data.Text as Text

import Data.Models (
  ReaderInfo (ReaderInfo, ipAddress, userAgent),
  LikeInfo (LikeInfo, readerInfo, postStringId))
import Database.Schema (
  BlogDB(_blogLikes, _blogPosts, _blogReaders),
  Like, 
  LikeT(Like, _likeReaderKey, _likePostKey),
  Post,
  PostT(Post, _postStringId),
  Reader, 
  ReaderT(Reader, _readerIpAddress, _readerUserAgent),
  blogDb)

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

getReaderKey :: ReaderInfo -> SqliteM (PrimaryKey ReaderT Identity)
getReaderKey (ReaderInfo { ipAddress, userAgent }) = do
  maybeReader <- findReader ipAddress userAgent
  case maybeReader of
    Just reader -> return $ pk reader
    Nothing -> insertNewReader ipAddress userAgent

runDB :: Connection -> SqliteM () -> IO ()
runDB = runBeamSqliteDebug putStrLn

incrementLikesCount :: Connection -> LikeInfo -> IO ()
incrementLikesCount conn (LikeInfo {readerInfo, postStringId }) = 
  withTransaction conn $ runDB conn $ do
    postKey <- getPostKey postStringId
    readerKey <- getReaderKey readerInfo
    insertNewLike readerKey postKey
  
