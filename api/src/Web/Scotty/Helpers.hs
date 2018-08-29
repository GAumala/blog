{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Scotty.Helpers (
  APIError (httpStatus, msg), 
  getIpAddress, 
  getUserAgent,
  whenValid) where

import Data.Text.Lazy (Text)
import Network.HTTP.Types.Status (Status, badRequest400, ok200, status500)
import Web.Scotty (ActionM, header, status, text) 

import Data.Models (
  IPAddress (IPAddress),
  UserAgent (UserAgent))

data APIError = APIError { httpStatus :: Status, msg :: Text }


getUserAgent :: ActionM (Either APIError UserAgent)
getUserAgent = do
  maybeUserAgent <- header "user-agent"  
  case maybeUserAgent of
    Just userAgent -> return $ Right $ UserAgent userAgent
    Nothing -> return $ Left $ APIError { 
                    httpStatus = badRequest400, 
                    msg = "Missing user agent header" }

getIpAddress :: ActionM (Either APIError IPAddress)
getIpAddress = do
  maybeIpAddress <- header "x-real-ip"  
  case maybeIpAddress of
    Just address -> return $ Right $ IPAddress address
    Nothing -> return $ Left $ APIError { 
                    httpStatus = status500, 
                    msg = "Missing headers from proxy server" }

whenValid :: Either APIError b -> (b -> ActionM ()) -> ActionM ()
whenValid (Left (APIError { httpStatus, msg })) _ = do 
  status httpStatus
  text msg
whenValid (Right value) fn = fn value

