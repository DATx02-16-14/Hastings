module Server.Lobby where

import Haste.App (SessionID)
import Control.Concurrent (modifyMVar_, newChan, readMVar, readChan)
import Data.List (find)
import Database.Persist (entityVal)

import qualified Hastings.Database.Player as PlayerDB
import qualified Hastings.Database.Fields as Fields
import Hastings.ServerUtils
import Server.Game as Game
import Server.Chat as Chat
import LobbyTypes
import Hastings.Utils

connect :: ConcurrentClientList -> Name -> SessionID -> IO ()
connect mVarClients name sid = do
  PlayerDB.saveOnlinePlayer name sid
  modifyMVar_ mVarClients  $ \clients -> do
    lobbyChannel <- newChan
    gameChannel <- newChan
    return $ clientEntry sid name lobbyChannel gameChannel : clients

  clientList <- readMVar mVarClients
  messageClients ClientJoined clientList

disconnect :: ConcurrentClientList -> GamesList-> SessionID -> IO ()
disconnect mVarClients mVarGames sid = do
  PlayerDB.deleteOnlinePlayer sid
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
  player <- PlayerDB.retrieveOnlinePlayer sid
  playerWithNewName <- PlayerDB.retrievePlayerByUsername newName
  case playerWithNewName of
    Just plr -> do
      let client = lookupClientEntry sid clientList
      case client of
        Just c  -> messageClients (LobbyError "That username already exists" ) [c]
        Nothing -> return ()
    Nothing  -> do
      PlayerDB.changeUserName (getName player) newName

      modifyMVar_ mVarClients $ \cs ->
        return $ updateNick sid cs
      modifyMVar_ mVarGames $ \gs ->
        return $ map (\(uuid, gameData) -> (uuid, gameData {players = updateNick sid $ players gameData})) gs

      -- Update the clients with this new information
      messageClients NickChange clientList
      -- Notify all chats about nick update
      Chat.notifyClientChats mVarClients sid $ getName player ++ " changed nick to " ++ newName

  where
    updateNick sid = updateListElem (\c -> c {name = newName}) (\c -> sid == sessionID c)
    getName = maybe "NO_SUCH_CLIENT" (Fields.playerUserName . entityVal)

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
