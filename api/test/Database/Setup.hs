module Database.Setup (setupDB) where

import System.Process (callCommand)

setupDB :: String -> IO ()
setupDB dbFileName = callCommand $ "./sql/setup.sh " ++ dbFileName
