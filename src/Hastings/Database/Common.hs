-- | Contains database utility functions.
module Hastings.Database.Common
   where

import qualified Database.Persist.MySQL as MySQL
import qualified Database.Esqueleto as Esql
import qualified Control.Monad.Logger as Logger
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import Hastings.Database.Fields
import Hastings.Config

-- |Run database migration, creating all relevant tables.
migrateDatabase :: IO ()
migrateDatabase = runDB $ Esql.runMigration migrateAll


hastingsConnectionInfo :: String -> MySQL.ConnectInfo
hastingsConnectionInfo pass = MySQL.defaultConnectInfo {
  MySQL.connectHost = databaseHostAddress,
  MySQL.connectPort = databaseHostPort,
  MySQL.connectUser = databaseUser,
  MySQL.connectPassword = pass,
  MySQL.connectDatabase = databaseName
}


-- |Helper function that should be called before running a query on the database.
--  It's primary purpose is running the proper prerequisites to be able to excecute an SQL query on the database.
--  It connects to a database with the following connection information
-- * Server on localhost
-- * User root
-- * No password
-- * Database hastings
-- * Character set utf8
--
-- The following is an example on how you would use this function when inserting a car with the brand \"Volvo\" to the database.
-- @
-- saveCar = runDB $ insert $ Car \"Volvo\"
-- @
runDB :: MySQL.SqlPersistT (Logger.NoLoggingT IO) a -> IO a
runDB l = do
  pass <- fromMaybe "" <$> lookupEnv "HASTINGS_DATABASE_PASSWORD"
  (Logger.runNoLoggingT . MySQL.withMySQLConn (hastingsConnectionInfo pass) . Esql.runSqlConn) l
