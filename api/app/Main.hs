{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Database.SQLite.Simple  as SQLite
import Web.Actions (getLikes, postLike)
import Web.Scotty (get, post, scotty, ActionM, ScottyM)

runBlogAPI :: SQLite.Connection -> ScottyM () 
runBlogAPI conn = do
  get "/likes" getLikes
  post "/likes/:stringId" $ postLike conn

main :: IO ()
main = do
  conn <- SQLite.open "api.sqlite"
  scotty 8008 $ runBlogAPI conn
