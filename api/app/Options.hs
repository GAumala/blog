{-# LANGUAGE NamedFieldPuns #-}

module Options (optionsFromConfig) where

import Data.String (fromString)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)
import Web.Scotty (verbose, settings, Options (Options))
import Data.Config (BlogConfig(BlogConfig), cfg_port)

mySettingsWithConfig :: BlogConfig -> Settings
mySettingsWithConfig (BlogConfig { cfg_port }) =  modify defaultSettings
  where modify = (setPort cfg_port) . (setHost $ fromString "127.0.0.1") 

optionsFromConfig :: BlogConfig -> Options
optionsFromConfig config = Options { 
  verbose = 0, 
  settings = mySettingsWithConfig config 
}
