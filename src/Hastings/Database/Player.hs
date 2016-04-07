-- | Contains database functions that manipulate 'OnlinePlayer' and 'Player'
module Hastings.Database.Player
   where

import Hastings.Database.Common
import Hastings.Database.Fields

import Database.Esqueleto
import Data.Word (Word64)
import Data.Maybe (listToMaybe)


-- |Save a player to the database.
savePlayer :: String -- ^The name of the player to save.
           -> IO (Key Player)
savePlayer name = runDB $ insert $ Player name

-- |Delete a player from the database.
deletePlayer :: String -- ^The username of the player to delete.
             -> IO ()
deletePlayer userName = runDB $ deleteBy $ UniqueUsername userName

-- |Retrieve a player from the database by their username.
retrievePlayerbyUsername :: String -- ^The username of the player to retrieve.
                         -> IO (Maybe (Entity Player))
retrievePlayerbyUsername name = runDB $ getBy $ UniqueUsername name

-- |Change the username of a player.
changeUserName :: String -- ^The old username.
               -> String -- ^The new username.
               -> IO (Key Player)
changeUserName oldName newName = do
  deletePlayer oldName
  savePlayer newName

-- |Save an new online player to the database.
--  Creates a new player with the specified username if the player doesn't exist.
saveOnlinePlayer :: String -- ^The name of the player to save.
                 -> Word64 -- ^The sessionID of the player to save.
                 -> IO (Key OnlinePlayer)
saveOnlinePlayer name sessionID = do
  player <- retrievePlayerbyUsername name
  case player of
    Just entity -> saveOnlinePlayer' sessionID (entityKey entity)
    _           -> savePlayer name >>= saveOnlinePlayer' sessionID

    where
      saveOnlinePlayer' sessionID key = runDB $ insert $ OnlinePlayer key sessionID


-- |Retrieve an online player from the database.
--
--  Runs this query on the database.
--
-- @
-- SELECT OnlinePlayer.*, Player.Id
-- WHERE OnlinePlayer.Player == Player.Id
-- @
retrieveOnlinePlayer :: Word64 -- ^The sessionID of the player to retrieve.
                     -> IO (Maybe (Entity Player))
retrieveOnlinePlayer sessionID = runDB $ do
  playerList <- select $
       from $ \(b, p) -> do
         where_ (b ^. OnlinePlayerPlayer ==. p ^. PlayerId)
         return p
  return $ listToMaybe playerList

-- |Delete an online player from the database.
deleteOnlinePlayer :: Word64 -- ^The sessionID of the player to delete.
                   -> IO ()
deleteOnlinePlayer sessionID = runDB $ deleteBy $ UniqueSession sessionID
