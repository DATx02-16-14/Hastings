
{-# LANGUAGE CPP #-}
-- |Contains all functions that are meant to be run server side only. As such this only needs to be compiled with GHC and never with Haste.
module Server(
    connect
  , disconnect
  , createGame
  , getGamesList
  , playerJoinGame
  , playerNamesInGameWithSid
  , getConnectedPlayerNames
  , leaveGame
  , kickPlayerWithSid
  , changeNickName
  , changeGameNameWithSid
  , findGameNameWithID
  , findGameNameWithSid
  , readLobbyChannel
  , changeMaxNumberOfPlayers
  , readChatChannel
  , sendChatMessage
  , joinChat
  , leaveChat
  , getJoinedChats
  , getChats
  , setPasswordToGame
  , isGamePasswordProtected
  , remoteIsOwnerOfGame
  ) where

import Haste.App

import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe
import Data.ByteString.Char8 (ByteString, empty, pack, unpack)

import LobbyTypes
import Hastings.Utils
import Hastings.ServerUtils
import qualified Server.Lobby as Lobby
import qualified Server.Game as Game
import qualified Server.Chat as Chat


-- |Initial connection with the server
-- Creates a 'Player' for that user given a name.
connect :: Server ConcurrentClientList -> Name -> Server ()
connect remoteClientList name = do
  mVarClients <- remoteClientList
  sid <- getSessionID

  liftIO $ Lobby.connect mVarClients name sid

-- |Disconnect client from server.
disconnect :: LobbyState -> SessionID -> Server()
disconnect (clientList, games, chats) sid = do
  mVarClients <- clientList
  mVarGames <- games
  cs <- clientList >>= liftIO . CC.readMVar
  sid <- getSessionID
  maybe
    (return ())
    (\c -> do
      notifyClientChats clientList $ name c ++ " disconnected"
      return ())
    $ sid `lookupClientEntry` cs

  liftIO $ Lobby.disconnect mVarClients mVarGames sid

-- |Removes a player that has disconnected from it's game
leaveGame :: Server ConcurrentClientList -> Server ()
leaveGame remoteClientList = do
  mVarClients <- remoteClientList
  sid <- getSessionID
  liftIO $ Game.leaveGame mVarClients sid

-- |Creates a new game on the server. The 'Int' represents the max number of players.
createGame :: Server ConcurrentClientList -> Int -> Server (Maybe String)
createGame remoteClientList maxPlayers = do
  mVarClients <- remoteClientList
  sid <- getSessionID
  liftIO $ Game.createGame mVarClients sid maxPlayers

-- |Returns a list of the each game's uuid as a String
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  return $ getUUIDFromGamesList gameList

-- |Lets a player join a 'LobbyGame'. The 'String' represents the UUID for the game.
-- |The second 'String' is the password for the game, if there is no password it can be left empty.
playerJoinGame :: Server ConcurrentClientList -> String -> String -> Server Bool
playerJoinGame remoteClientList gameID passwordString = do
  mVarClients <- remoteClientList
  sid <- getSessionID
  liftIO $ Game.playerJoinGame mVarClients sid gameID passwordString

-- |Finds the name of a game given it's identifier
findGameNameWithID :: Server GamesList -> String -> Server String
findGameNameWithID remoteGames gid = do
  mVarGames <- remoteGames
  liftIO $ Game.findGameNameWithID mVarGames gid

-- |Finds the name of the game the client is currently in
findGameNameWithSid :: Server GamesList -> Server String
findGameNameWithSid remoteGames = do
  mVarGames <- remoteGames
  sid <- getSessionID
  liftIO $ Game.findGameNameWithSid mVarGames sid

-- |Finds the name of the players of the game the current client is in
playerNamesInGameWithSid :: Server GamesList -> Server [String]
playerNamesInGameWithSid remoteGames = do
  mVarGames <- remoteGames
  sid <- getSessionID
  liftIO $ Game.playerNamesInGameWithSid mVarGames sid

-- |Returns a list of strings containing all connected players names.
getConnectedPlayerNames :: Server ConcurrentClientList -> Server [String]
getConnectedPlayerNames remoteClientList = do
  mVarClients <- remoteClientList
  liftIO $ Lobby.getConnectedPlayerNames mVarClients

-- |Kicks the player with index 'Int' from the list of players in
-- the game that the current client is in.
kickPlayerWithSid :: Server GamesList
                  -> Int  -- ^The index in the list of players of the player to kick
                  -> Server ()
kickPlayerWithSid remoteGames clientIndex = do
  mVarGamesList <- remoteGames
  sid <- getSessionID
  liftIO $ Game.kickPlayerWithSid mVarGamesList sid clientIndex

-- |Change the nick name of the current player to that given.
changeNickName :: Server ConcurrentClientList -> Server GamesList -> Name -> Server ()
changeNickName remoteClientList remoteGames newName = do
  mVarClients <- remoteClientList
  mVarGames <- remoteGames
  sid <- getSessionID
  liftIO $ Lobby.changeNickName mVarClients mVarGames sid newName

-- | Sends a server notification to all chats the client has joined
notifyClientChats :: Server ConcurrentClientList -> String -> Server ()
notifyClientChats remoteClients notification = do
  mVarClients <- remoteClients
  sid <- getSessionID
  liftIO $ Chat.notifyClientChats mVarClients sid notification

-- |Change the name of a 'LobbyGame' that the connected client is in
changeGameNameWithSid :: Server GamesList -> Server ConcurrentClientList -> Name -> Server ()
changeGameNameWithSid remoteGames remoteClients newName = do
  mVarClients <- remoteClients
  mVarGames <- remoteGames
  sid <- getSessionID
  liftIO $ Game.changeGameNameWithSid mVarGames mVarClients sid newName

-- |Reads the lobby channel of the current client and returns the message.
-- |Blocking method if the channel is empty
readLobbyChannel :: Server ConcurrentClientList -> Server LobbyMessage
readLobbyChannel remoteClientList = do
  mVarClients <- remoteClientList
  sid <- getSessionID
  liftIO $ Lobby.readLobbyChannel mVarClients sid

-- |Changes the maximum number of players for a game
-- Requires that the player is the last in the player list (i.e. the owner)
changeMaxNumberOfPlayers :: Server GamesList -> Int -> Server ()
changeMaxNumberOfPlayers remoteGames newMax = do
  mVarGamesList <- remoteGames
  sid <- getSessionID
  liftIO $ Game.changeMaxNumberOfPlayers mVarGamesList sid newMax

-- |Returns if the current player is owner of the game it's in
remoteIsOwnerOfGame :: Server GamesList -> Server Bool
remoteIsOwnerOfGame remoteGames = do
  gamesList <- remoteGames >>= liftIO . CC.readMVar
  sid <- getSessionID
  return $ isOwnerOfGame sid gamesList

-- |Called by client to join a chat
joinChat :: Server ConcurrentClientList -> Server ConcurrentChatList -> String -> Server ()
joinChat remoteClientList remoteChatList chatName = do
  sid <- getSessionID
  mVarClients <- remoteClientList
  mVarChats <- remoteChatList
  liftIO $ Chat.joinChat mVarClients mVarChats sid chatName


-- | Sends a ChatAnnounceJoin to all clients present in the channel.
-- | String is the name of the channel joined
announceChatJoin :: Server ConcurrentClientList -> Server ConcurrentChatList -> String -> Server ()
announceChatJoin remoteClientList remoteChatList chatName = do
  mVarClients <- remoteClientList
  mVarChats <- remoteChatList
  sid <- getSessionID
  liftIO $ Chat.announceChatJoin mVarClients mVarChats sid chatName

-- | Called by client to leave the named Chat
-- | String is the name of the chat to be left
leaveChat :: Server ConcurrentClientList -> String -> Server ()
leaveChat remoteClientList chatName = do
  sid <- getSessionID
  mVarClients <- remoteClientList
  liftIO $ Chat.leaveChat mVarClients sid chatName

-- |Called by a client to read its various chat channels
readChatChannel :: Server ConcurrentClientList -> String ->  Server ChatMessage
readChatChannel remoteClientList chatName = do
  sid <- getSessionID
  mVarClients <- remoteClientList
  liftIO $ Chat.readChatChannel mVarClients sid chatName

-- | Called by the client to send a chat message
sendChatMessage :: Server ConcurrentClientList -> Server ConcurrentChatList -> String -> ChatMessage -> Server ()
sendChatMessage remoteClientList remoteChatList chatName chatMessage = do
  sid <- getSessionID
  mVarClients <- remoteClientList
  mVarChats <- remoteChatList
  liftIO $ Chat.sendChatMessage mVarClients mVarChats sid chatName chatMessage

-- | Return list of chatnames which the client have joined
getJoinedChats :: Server ConcurrentClientList -> Server [String]
getJoinedChats remoteClientList = do
  sid <- getSessionID
  mVarClients <- remoteClientList
  liftIO $ Chat.getJoinedChats mVarClients sid

-- | Return list of all chatnames
getChats :: Server ConcurrentChatList -> Server [String]
getChats remoteChatList = do
  mVarChats <- remoteChatList
  liftIO $ Chat.getChats mVarChats

-- |Sets the password (as a 'ByteString') of the game the client is in.
-- |Only possible if the client is the owner of the game.
setPasswordToGame :: Server GamesList -> String -> Server ()
setPasswordToGame remoteGames passwordString = do
  mVarGames <- remoteGames
  sid <- getSessionID
  liftIO $ Game.setPasswordToGame mVarGames sid passwordString

-- |Returns True if game is password protected, False otherwise. 'String' is the UUID of the game
isGamePasswordProtected :: Server GamesList -> String -> Server Bool
isGamePasswordProtected remoteGames guuid = do
  mVarGames <- remoteGames
  liftIO $ Game.isGamePasswordProtected mVarGames guuid
