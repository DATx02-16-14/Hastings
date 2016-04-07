module Server.Lobby where

import Haste.App (SessionID)
import Control.Concurrent (modifyMVar_, newChan, readMVar, readChan)
import Data.List (find)

import Hastings.ServerUtils
import Server.Game as Game
import Server.Chat as Chat
import LobbyTypes
import Hastings.Utils

connect :: ConcurrentClientList -> Name -> SessionID -> IO ()
connect mVarClients name sid = do
  modifyMVar_ mVarClients  $ \clients -> do
    lobbyChannel <- newChan
    return $ ClientEntry sid name [] lobbyChannel : clients

  clientList <- readMVar mVarClients
  messageClients ClientJoined clientList

disconnect :: ConcurrentClientList -> GamesList-> SessionID -> IO ()
disconnect mVarClients mVarGames sid = do
  disconnectPlayerFromLobby mVarClients sid
  Game.leaveGame mVarGames sid

  clients <- readMVar mVarClients
  messageClients ClientLeft clients

-- |Removes a player that has disconnected from player list
disconnectPlayerFromLobby :: ConcurrentClientList -> SessionID -> IO ()
disconnectPlayerFromLobby mVarClients sid =
  modifyMVar_ mVarClients $ \cs ->
    return $ filter ((sid /=) . sessionID) cs

-- |Retreives the names of all players connected to the Lobby
getConnectedPlayerNames :: ConcurrentClientList -> IO [String]
getConnectedPlayerNames mVarClients = do
  clientList <- readMVar mVarClients
  return $ map name clientList

-- |Changes the nick name currently connected player
changeNickName :: ConcurrentClientList -> GamesList -> SessionID -> Name -> IO ()
changeNickName mVarClients mVarGames sid newName = do
  clientList <- readMVar mVarClients
  let oldName = maybe
                  "NO_SUCH_CLIENT"
                  name
                  $ sid `lookupClientEntry` clientList

  modifyMVar_ mVarClients $ \cs ->
    return $ updateNick sid cs
  modifyMVar_ mVarGames $ \gs ->
    return $ map (\(uuid, gameData) -> (uuid, gameData {players = updateNick sid $ players gameData})) gs

  -- Update the clients with this new information
  messageClients NickChange clientList
  -- Notify all chats about nick update
  Chat.notifyClientChats mVarClients sid $ oldName ++ " changed nick to " ++ newName

  where
    updateNick sid = updateListElem (\c -> c {name = newName}) (\c -> sid == sessionID c)

-- |Reads the lobby channel of the current client and returns the message.
-- |Blocking method if the channel is empty
readLobbyChannel :: ConcurrentClientList -> SessionID -> IO LobbyMessage
readLobbyChannel mVarClients sid = do
  clients <- readMVar mVarClients
  case find (\c -> sessionID c == sid) clients of
    Just client -> readChan $ lobbyChannel client
    Nothing     -> do
      print "readLobbyChannel: Could not find session ID"
      return $ LobbyError "Could not find session"
