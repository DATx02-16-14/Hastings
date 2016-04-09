module Hastings.Database.Game where

import Hastings.Database.Common (runDB)
import Hastings.Database.Fields

import qualified Database.Esqueleto as Esql

-- |Retrieve a game from the database.
retrieveGameByName :: String -- ^The name of the game to retrieve.
             -> IO (Maybe (Esql.Entity Game))
retrieveGameByName name = runDB $ Esql.getBy $ UniqueName name

-- |Save a game to the database.
saveGame :: String -- ^The name of the game to save.
         -> Int    -- ^Maximum amount of players for this game.
         -> IO (Esql.Key Game)
saveGame name maxAmountOfPlayers = runDB $ Esql.insert $ Game name maxAmountOfPlayers

