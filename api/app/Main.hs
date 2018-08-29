{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (forM_)
import qualified Database.SQLite.Simple  as SQLite
import Web.Scotty (defaultHandler, get, post, text, scottyOpts, ActionM, ScottyM)
import Debug.Trace

import Data.Config (
  BlogConfig(BlogConfig), 
  loadConfig,
  )
import Options (optionsFromConfig)
import Web.Actions (errorHandler, getLikes, postLike)
import Web.RoutesContext (
  RoutesContext(RoutesContext), 
  ctx_conn, 
  ctx_maybeCIScript,
  initRoutesContext
  )

updateStaticContent :: ActionM () 
updateStaticContent = text "ok"

runBlogAPI :: RoutesContext -> ScottyM () 
runBlogAPI ctx = do
  let RoutesContext { 
    ctx_conn = conn, 
    ctx_maybeCIScript = maybeCIScript 
    } = ctx

  get "/likes/:stringId" $ getLikes conn
  post "/like/:stringId" $ postLike conn
  forM_ maybeCIScript $ \ ciScript ->
    post "/ci-hook" $ updateStaticContent

  defaultHandler errorHandler


main :: IO ()
main = do
  config <- loadConfig
  ctx <- initRoutesContext config
  let options = optionsFromConfig config

  scottyOpts options $ runBlogAPI ctx
