{-# LANGUAGE NamedFieldPuns #-}

module Web.RoutesContext (
  RoutesContext(RoutesContext), 
  ctx_conn, 
  ctx_maybeContentDir, 
  initRoutesContext
) where

import qualified Database.SQLite.Simple  as SQLite
import Data.Config (BlogConfig(BlogConfig), 
  cfg_dbFile, 
  cfg_maybeContentDir
  )

data RoutesContext = RoutesContext { 
  ctx_conn :: SQLite.Connection, 
  ctx_maybeContentDir :: Maybe FilePath 
}

initRoutesContext :: BlogConfig -> IO RoutesContext
initRoutesContext (BlogConfig { cfg_dbFile, cfg_maybeContentDir }) = do
  conn <- SQLite.open cfg_dbFile
  return RoutesContext { 
    ctx_conn = conn, 
    ctx_maybeContentDir = cfg_maybeContentDir 
  }
