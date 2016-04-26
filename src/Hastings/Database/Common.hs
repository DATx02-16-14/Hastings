-- | Contains database utility functions.
module Hastings.Database.Common
   where

import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.Sql as Pers
import qualified Database.Esqueleto as Esql
import qualified Control.Monad.Logger as Logger
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import Data.Word (Word16)

import Hastings.Database.Fields
import Hastings.Config

-- |Run database migration, creating all relevant tables.
migrateDatabase :: IO ()
migrateDatabase = runDB $ Esql.runMigration migrateAll

-- |Clear all tables in database.
clearDatabase :: IO ()
clearDatabase = runDB $ do
  Pers.deleteWhere ([] :: [Pers.Filter OnlinePlayer])
  Pers.deleteWhere ([] :: [Pers.Filter PlayerInGame])
  Pers.deleteWhere ([] :: [Pers.Filter Player])
  Pers.deleteWhere ([] :: [Pers.Filter Game])

-- | Create the ConnectInfo
hastingsConnectionInfo :: String -- ^Database host address
                       -> Word16 -- ^Database host port
                       -> String -- ^Database user
                       -> String -- ^Database password
                       -> String -- ^Database name
                       -> MySQL.ConnectInfo
hastingsConnectionInfo host port user pass dbName = MySQL.defaultConnectInfo {
  MySQL.connectHost = host,
  MySQL.connectPort = port,
  MySQL.connectUser = user,
  MySQL.connectPassword = pass,
  MySQL.connectDatabase = dbName
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
    address <- fromMaybe "localhost" <$> lookupEnv "HASTINGS_DATABASE_HOST_ADDRESS"
    port    <- fromMaybe "3306" <$> lookupEnv "HASTINGS_DATABASE_HOST_PORT"
    user    <- fromMaybe "root" <$> lookupEnv "HASTINGS_DATABASE_USER"
    pass    <- fromMaybe "" <$> lookupEnv "HASTINGS_DATABASE_PASSWORD"
    name    <- fromMaybe "hastings" <$> lookupEnv "HASTINGS_DATABASE_NAME"
    let connectionInfo = hastingsConnectionInfo address (read port :: Word16) user pass name
    (Logger.runNoLoggingT . MySQL.withMySQLConn connectionInfo . Esql.runSqlConn) l
