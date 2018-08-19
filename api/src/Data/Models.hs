
module Data.Models (
  IPAddress (IPAddress, unwrapIPAddress),
  ReaderInfo (ReaderInfo, ipAddress, userAgent),
  LikeInfo (LikeInfo, readerInfo, postStringId),
  UserAgent (UserAgent, unwrapUserAgent)
) where

import Data.Text.Lazy (Text)

newtype UserAgent = UserAgent { unwrapUserAgent :: Text } deriving (Show)
newtype IPAddress = IPAddress { unwrapIPAddress :: Text } deriving (Show)

data ReaderInfo = ReaderInfo { ipAddress :: IPAddress, userAgent :: UserAgent }
data LikeInfo = LikeInfo { postStringId :: Text, readerInfo :: ReaderInfo }
