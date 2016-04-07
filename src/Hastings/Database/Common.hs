-- | Contains database utility functions.
module Hastings.Database.Common
   where

import qualified Database.Persist.MySQL as MySQL
import Database.Esqueleto
import Control.Monad.Logger

import Hastings.Database.Fields

-- |Run database migration, creating all relevant tables.
migrateDatabase :: IO ()
migrateDatabase = runDB $ runMigration migrateAll

-- |Helper function that should be called before running a query on the database.
--
-- @
-- saveCar = runDB $ insert $ Car \"Volvo\"
-- @
runDB :: SqlPersistT (NoLoggingT IO) a -> IO a
runDB = runNoLoggingT . MySQL.withMySQLConn MySQL.defaultConnectInfo . runSqlConn
