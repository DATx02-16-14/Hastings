-- |Contains util functions for the module "LobbyServer".
-- The functions are meant to be as pure as possible
module Hastings.ServerUtils where

import Haste.App (SessionID)

import LobbyTypes (Name, ClientEntry, name, LobbyMessage, lobbyChannel, sessionID)
import Hastings.Utils (updateListElem)
import qualified Hastings.Database.Game as GameDB
import qualified Hastings.Database.Fields as Fields
import qualified Database.Esqueleto as Esql

import qualified Control.Concurrent as CC (writeChan)
import Data.List (find, nub)

-- |Get's the uuid from a list of lobby games
getUUIDFromGamesList :: [Esql.Entity Fields.Game] -> [String]
getUUIDFromGamesList = map $ Fields.gameUuid . Esql.entityVal

-- |Finds the client with 'Name' from the list of 'ClientEntry'
findClient :: Name -> [ClientEntry] -> Maybe ClientEntry
findClient clientName = find ((clientName ==).name)

-- |Maps over the clients and writes the message to their channel
messageClients :: LobbyMessage -> [ClientEntry] -> IO ()
messageClients m = mapM_ (\c -> CC.writeChan (lobbyChannel c) m)

-- |Messages all clients in the list of sessionIDs
messageClientsWithSid :: LobbyMessage -> [ClientEntry] -> [SessionID] -> IO ()
messageClientsWithSid message clientEntries sessionIDs =
  messageClients message $ filter (\c -> sessionID c `elem` sessionIDs) clientEntries

-- |Returns if the current player is owner of the game it's in
isOwnerOfGame :: SessionID -> IO Bool
isOwnerOfGame sid = do
  dbGame <- GameDB.retrieveGameBySid sid
  case dbGame of
    Just (Esql.Entity _ game) -> return $ Fields.gameOwner game == sid
    _                         -> return False

-- |Function that finds a 'ClientEntry' based on the 'SessionID'
lookupClientEntry :: SessionID -> [ClientEntry] -> Maybe ClientEntry
lookupClientEntry sid = find ((sid ==) . sessionID)
