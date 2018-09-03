{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Scotty.Helpers (
  APIError (httpStatus, msg), 
  getIpAddress, 
  getUserAgent,
  truncateUserInput,
  whenValid) where

import qualified Data.Text.Lazy as T
import Network.HTTP.Types.Status (Status, badRequest400, status500)
import Web.Scotty (ActionM, header, status, text) 

import Data.Models (
  IPAddress (IPAddress),
  UserAgent (UserAgent))

data APIError = APIError { httpStatus :: Status, msg :: T.Text }

truncateUserInput :: T.Text -> T.Text
truncateUserInput input = truncated
  where maxLength =  140
        truncated = if T.length input < maxLength 
                        then input 
                        else T.take maxLength input `mappend` "..." 

getUserAgent :: ActionM (Either APIError UserAgent)
getUserAgent = do
  maybeUserAgent <- header "user-agent"  
  case maybeUserAgent of
    Just userAgent -> return $ Right $ UserAgent $ truncateUserInput userAgent
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

