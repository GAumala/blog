module Options (myOptions) where

import Data.String (fromString)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)
import Web.Scotty (verbose, settings, Options (Options))

mySettings :: Settings
mySettings =  modify defaultSettings
  where modify = (setPort 8008) . (setHost $ fromString "127.0.0.1") 

myOptions :: Options
myOptions = Options { verbose = 0, settings = mySettings }
