{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types.Status (ok200)
import qualified Database.SQLite.Simple  as SQLite
import Web.Scotty (get, json, param, post, scotty, status, text, ActionM, 
  ScottyM)

import Database.Queries (incrementLikesCount)
import Data.Models (
  ReaderInfo (ReaderInfo, ipAddress, userAgent),
  LikeInfo (LikeInfo, readerInfo, postStringId))

getLikes :: ActionM ()
getLikes = text "0"

postLike :: ActionM ()
postLike = status ok200

runBlogAPI :: ScottyM () 
runBlogAPI = do
  get "/likes" getLikes
  post "/likes" postLike

main :: IO ()
main = do
  conn <- SQLite.open "api.sqlite"
  let newReaderInfo = ReaderInfo { 
    ipAddress = "192.168.0.1", 
    userAgent = "Mozilla/Linux/Firefox 57" }
  let newLikeInfo = LikeInfo { 
    postStringId = "2018-08-16", 
    readerInfo = newReaderInfo }
  incrementLikesCount conn newLikeInfo 
  scotty 8008 runBlogAPI
