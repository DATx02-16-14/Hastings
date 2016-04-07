-- | Contains database utility functions.
module Hastings.Database.Common
   where

import qualified Database.Persist.MySQL as MySQL
import qualified Database.Esqueleto as Esql
import qualified Control.Monad.Logger as Logger

import Hastings.Database.Fields

-- |Run database migration, creating all relevant tables.
migrateDatabase :: IO ()
migrateDatabase = runDB $ Esql.runMigration migrateAll

-- |Helper function that should be called before running a query on the database.
--  It's primary purpose is running the proper prerequisites to be able to excecute an SQL query on the database.
--  It connects to a database with the following connection information
-- * Server on localhost
-- * User root
-- * No password
-- * Database test
-- * Character set utf8
--
-- The following is an example on how you would use this function when inserting a car with the brand \"Volvo\" to the database.
-- @
-- saveCar = runDB $ insert $ Car \"Volvo\"
-- @
runDB :: MySQL.SqlPersistT (Logger.NoLoggingT IO) a -> IO a
runDB = Logger.runNoLoggingT . MySQL.withMySQLConn MySQL.defaultConnectInfo . Esql.runSqlConn
