-- |Contains util functions for the module "LobbyServer".
-- The functions are meant to be as pure as possible
module Hastings.ServerUtils where

import Haste.App (SessionID)

import LobbyTypes (LobbyGame, Name, ClientEntry, name, LobbyMessage, players, lobbyChannel, sessionID)
import Hastings.Utils (updateListElem)

import qualified Control.Concurrent as CC (writeChan)
import Data.List (find, nub)

-- |Get's the uuid from a list of lobby games
getUUIDFromGamesList :: [LobbyGame] -> [String]
getUUIDFromGamesList = map fst

-- |Finds the client with 'Name' from the list of 'ClientEntry'
findClient :: Name -> [ClientEntry] -> Maybe ClientEntry
findClient clientName = find ((clientName ==).name)

-- |Maps over the clients and writes the message to their channel
messageClients :: LobbyMessage -> [ClientEntry] -> IO ()
messageClients m = mapM_ (\c -> CC.writeChan (lobbyChannel c) m)

-- |Deletes the player with 'Int' from the game.
deletePlayerFromGame :: Int  -- ^The index of the player to kick
                     -> LobbyGame  -- ^The game to kick the player from
                     -> LobbyGame
deletePlayerFromGame clientIndex (gameID, gameData)  =
  (gameID, gameData {players = clientHead ++ clientTail})
  where
    (clientHead, c:clientTail) = splitAt clientIndex $ players gameData

-- |Adds a player to a lobby game with the game ID
-- Doesn't care if the max #players has been reached.
addPlayerToGame :: ClientEntry -> String -> [LobbyGame] -> [LobbyGame]
addPlayerToGame client gameID =
  updateListElem (\(gID, gameData) -> (gID, gameData {players = nub $ client : players gameData})) ((gameID ==) .fst)

-- |Finds the 'LobbyGame' matching the first parameter and returns it
findGameWithID :: String -> [LobbyGame] -> Maybe LobbyGame
findGameWithID gid = find (\g -> fst g == gid)

-- |Finds the 'LobbyGame' that the current connection is in (or the first if there are multiple)
findGameWithSid :: SessionID -> [LobbyGame] -> Maybe LobbyGame
findGameWithSid sid = find (\(_, gameData) -> sid `elem` sidsInGame gameData)
  where
    sidsInGame gameData = map sessionID $ players gameData

-- |Returns if the current player is owner of the game it's in
isOwnerOfGame :: SessionID -> [LobbyGame] -> Bool
isOwnerOfGame sid gamesList =
  case findGameWithSid sid gamesList of
    Nothing         -> False
    Just (_, gData) -> sessionID (last $ players gData) == sid

-- |Function that finds a 'ClientEntry' based on the 'SessionID'
lookupClientEntry :: SessionID -> [ClientEntry] -> Maybe ClientEntry
lookupClientEntry sid = find ((sid ==) . sessionID)