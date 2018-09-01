{-# LANGUAGE OverloadedStrings #-}

module Database.QueriesSpec where

import Control.Exception.Base (bracket)
import qualified Database.SQLite.Simple  as SQLite
import System.Directory (removeFile)
import Test.Hspec

import Database.Queries (incrementLikesCount, getLikesCount)
import Database.Setup (setupDB)
import Data.Models (
  IPAddress (IPAddress, unwrapIPAddress),
  ReaderInfo (ReaderInfo, ipAddress, userAgent),
  LikeInfo (LikeInfo, readerInfo, postStringId),
  UserAgent (UserAgent, unwrapUserAgent)) 

testDBName = ".test.sqlite"

setupDBConnection :: IO SQLite.Connection 
setupDBConnection = do
  setupDB testDBName
  SQLite.open testDBName

tearDBConnection :: SQLite.Connection -> IO ()
tearDBConnection conn = do
  SQLite.close conn
  removeFile testDBName

withDatabaseConnection :: (SQLite.Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket setupDBConnection tearDBConnection

spec :: Spec
spec = around withDatabaseConnection $
  describe "Database.Queries module" $ 
    it "`getLikesCount` should return the total amount inserted with `incrementLikesCount`" $ \conn -> do
      let reader1 = ReaderInfo { 
            ipAddress = IPAddress "192.168.0.11", 
            userAgent = UserAgent "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/60.0"  
          }
          reader2 = ReaderInfo { 
            ipAddress = IPAddress "192.168.0.11", 
            userAgent = UserAgent "Mozilla/5.0 (Linux; Android 4.0.4; Galaxy Nexus Build/IMM76B) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.133 Mobile Safari/535.19"  
          }
          reader3 = ReaderInfo { 
            ipAddress = IPAddress "192.168.0.15", 
            userAgent = UserAgent "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/60.0"  
          }

          post1StringId = "2018-04-21"
          post2StringId = "2018-05-15"

          like1ToPost1 = LikeInfo {
            readerInfo = reader1,
            postStringId = post1StringId
          }
          like2ToPost1 = LikeInfo {
            readerInfo = reader1,
            postStringId = post1StringId
          }
          like3ToPost1 = LikeInfo {
            readerInfo = reader2,
            postStringId = post1StringId
          }
          like1ToPost2 = LikeInfo {
            readerInfo = reader3,
            postStringId = post2StringId
          }

      incrementLikesCount conn like1ToPost1
      incrementLikesCount conn like2ToPost1
      incrementLikesCount conn like1ToPost2
      incrementLikesCount conn like3ToPost1

      post1TotalLikes <- getLikesCount conn post1StringId
      post2TotalLikes <- getLikesCount conn post2StringId

      post1TotalLikes `shouldBe` Just 3
      post2TotalLikes `shouldBe` Just 1

      
    
