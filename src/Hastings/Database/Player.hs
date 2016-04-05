module Hastings.Database.Player
   where

import Hastings.Database.Common
import Hastings.Database.Fields

import Database.Esqueleto

-- |Save a game to the database.
savePlayer :: String -- ^The name of the player to save.
           -> String -- ^The sessionID of the player
           -> IO (Key Player)
savePlayer name sessionID = runDB $ insert $ Player name sessionID

-- |Retrieve a game from the database.
retrievePlayer :: String -- ^The sessionID of the player to retrieve.
               -> IO [Entity Player]
retrievePlayer sessionID = runDB $
  select $
    from $ \p -> do
      where_ (p ^. PlayerSession ==. val sessionID)
      return p

