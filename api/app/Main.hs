{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Database.SQLite.Simple  as SQLite
import Web.Actions (errorHandler, getLikes, postLike)
import Web.Scotty (defaultHandler, get, post, scottyOpts, ActionM, ScottyM)
import Options (myOptions)

runBlogAPI :: SQLite.Connection -> ScottyM () 
runBlogAPI conn = do
  get "/likes/:stringId" $ getLikes conn
  post "/like/:stringId" $ postLike conn
  defaultHandler errorHandler

main :: IO ()
main = do
  conn <- SQLite.open "api.sqlite"
  scottyOpts myOptions $ runBlogAPI conn
