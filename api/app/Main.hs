{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (defaultHandler, get, post, scottyOpts, ScottyM)

import Data.Config (loadConfig)
import Options (optionsFromConfig)
import Web.Actions (errorHandler, getLikes, postLike, updateStaticContent)
import Data.Foldable (forM_)
import Web.RoutesContext (
  RoutesContext(RoutesContext), 
  ctx_conn, 
  ctx_maybeContentDir,
  initRoutesContext
  )

runBlogAPI :: RoutesContext -> ScottyM () 
runBlogAPI ctx = do
  let RoutesContext { 
    ctx_conn = conn, 
    ctx_maybeContentDir = maybeContentDir 
    } = ctx

  get "/likes/:stringId" $ getLikes conn
  post "/like/:stringId" $ postLike conn
  forM_ maybeContentDir $ \ contentDir ->
    post "/ci-hook" $ updateStaticContent contentDir

  defaultHandler errorHandler


main :: IO ()
main = do
  config <- loadConfig
  ctx <- initRoutesContext config
  let options = optionsFromConfig config

  scottyOpts options $ runBlogAPI ctx
