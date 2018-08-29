{-# LANGUAGE NamedFieldPuns #-}

module Web.RoutesContext (
  RoutesContext(RoutesContext), 
  ctx_conn, 
  ctx_maybeCIScript, 
  initRoutesContext
) where

import qualified Database.SQLite.Simple  as SQLite
import Data.Config (BlogConfig(BlogConfig), 
  cfg_dbFile, 
  cfg_maybeCIScript
  )

data RoutesContext = RoutesContext { 
  ctx_conn :: SQLite.Connection, 
  ctx_maybeCIScript :: Maybe FilePath 
}

initRoutesContext :: BlogConfig -> IO RoutesContext
initRoutesContext (BlogConfig { cfg_dbFile, cfg_maybeCIScript }) = do
  conn <- SQLite.open cfg_dbFile
  return RoutesContext { 
    ctx_conn = conn, 
    ctx_maybeCIScript = cfg_maybeCIScript 
  }
