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

-- |Deletes the player with 'Name' from the game.
deletePlayerFromGame :: Name -> LobbyGame -> LobbyGame
deletePlayerFromGame clientName (gameID, gameData)  =
  (gameID, gameData {players = filter ((clientName /=) . name) $ players gameData})

-- |Adds a player to a lobby game with the game ID
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
