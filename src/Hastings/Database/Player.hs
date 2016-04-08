-- | Contains database functions that manipulate 'OnlinePlayer' and 'Player'
module Hastings.Database.Player
   where

import Hastings.Database.Common (runDB)
import Hastings.Database.Fields

import qualified Database.Esqueleto as Esql
import qualified Database.Persist.Sql as Pers

import Data.Word (Word64)
import Data.Maybe (listToMaybe)


-- |Save a player to the database.
savePlayer :: String -- ^The name of the player to save.
           -> IO (Esql.Key Player)
savePlayer name = runDB $ Esql.insert $ Player name

-- |Delete a player from the database.
deletePlayer :: String -- ^The username of the player to delete.
             -> IO ()
deletePlayer userName = runDB $ Esql.deleteBy $ UniqueUsername userName

-- |Retrieve a player from the database by their username.
retrievePlayerByUsername :: String -- ^The username of the player to retrieve.
                         -> IO (Maybe (Esql.Entity Player))
retrievePlayerByUsername name = runDB $ Esql.getBy $ UniqueUsername name

-- |Change the username of a player.
changeUserName :: String -- ^The old username.
               -> String -- ^The new username.
               -> IO ()
changeUserName oldName newName = runDB $ do
  Esql.update $ \player -> do
    Esql.set player [PlayerUserName Esql.=. Esql.val newName]
    Esql.where_ (player Esql.^. PlayerUserName Esql.==. Esql.val oldName)

-- |Save an new online player to the database.
--  Creates a new player with the specified username if the player doesn't exist.
saveOnlinePlayer :: String -- ^The name of the player to save.
                 -> Word64 -- ^The sessionID of the player to save.
                 -> IO (Esql.Key OnlinePlayer)
saveOnlinePlayer name sessionID = do
  player <- retrievePlayerByUsername name
  case player of
    Just entity -> saveOnlinePlayer' sessionID (Esql.entityKey entity)
    _           -> savePlayer name >>= saveOnlinePlayer' sessionID

    where
      saveOnlinePlayer' sessionID key = runDB $ Esql.insert $ OnlinePlayer key sessionID


-- |Retrieve an online player from the database.
--
--  Runs this query on the database.
--
-- @
-- SELECT OnlinePlayer.*, Player.Id
-- WHERE OnlinePlayer.Player == Player.Id
-- @
retrieveOnlinePlayer :: Word64 -- ^The sessionID of the player to retrieve.
                     -> IO (Maybe (Esql.Entity Player))
retrieveOnlinePlayer sessionID = runDB $ do
  playerList <- Esql.select $
       Esql.from $ \(b, p) -> do
         Esql.where_ (b Esql.^. OnlinePlayerPlayer Esql.==. p Esql.^. PlayerId)
         return p
  return $ listToMaybe playerList

-- |Delete an online player from the database.
deleteOnlinePlayer :: Word64 -- ^The sessionID of the player to delete.
                   -> IO ()
deleteOnlinePlayer sessionID = runDB $ Esql.deleteBy $ UniqueSession sessionID

-- |Delete all entries in the online players table
clearOnlinePlayers :: IO ()
clearOnlinePlayers = runDB $ Pers.deleteWhere ([] :: [Pers.Filter OnlinePlayer])