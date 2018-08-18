
module Data.Models (
ReaderInfo (ReaderInfo, ipAddress, userAgent),
LikeInfo (LikeInfo, readerInfo, postStringId)
) where

import Data.Text (Text)

data ReaderInfo = ReaderInfo { ipAddress :: Text, userAgent :: Text }
data LikeInfo = LikeInfo { postStringId :: Text, readerInfo :: ReaderInfo }
