module Hastings.Database.Player
   where

import Hastings.Database.Common
import Hastings.Database.Fields

import Database.Esqueleto
import Data.Word (Word64)

-- |Save a player to the database.
savePlayer :: String -- ^The name of the player to save.
           -> Word64 -- ^The sessionID of the player
           -> IO (Key Player)
savePlayer name sessionID = runDB $ insert $ Player name sessionID

-- |Retrieve a player from the database.
retrievePlayer :: Word64 -- ^The sessionID of the player to retrieve.
               -> IO (Maybe Player)
retrievePlayer sessionID = runDB $ do
  t <- getBy $ UniqueSession sessionID
  case t of
    Just entity -> return $ Just $ entityVal entity
    _           -> return Nothing