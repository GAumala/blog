{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types.Status (ok200)
import qualified Database.SQLite.Simple  as SQLite
import Web.Scotty (get, json, param, post, scotty, status, text, ActionM, 
  ScottyM)

import DB (incrementLikesCount)

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
  incrementLikesCount conn "192.168.0.1" "Mozilla/Linux/Firefox 57" "2018-08-16"
  scotty 8008 runBlogAPI
