
{-# LANGUAGE CPP #-}
-- |Contains all functions that are meant to be run server side only. As such this only needs to be compiled with GHC and never with Haste.
module LobbyServer(
  connect,
  disconnect,
  createGame,
  getGamesList,
  playerJoinGame,
  playerNamesInGameWithSid,
  getConnectedPlayerNames,
  disconnectPlayerFromLobby,
  disconnectPlayerFromGame,
  kickPlayerWithSid,
  changeNickName,
  changeGameNameWithSid,
  findGameNameWithID,
  findGameNameWithSid,
  readLobbyChannel,
  changeMaxNumberOfPlayers,
  readChatChannel,
  sendChatMessage,
  joinChat,
  getClientName) where

import Haste.App
import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe

import LobbyTypes
import Hastings.Utils
import Hastings.ServerUtils

#ifndef __HASTE__
import Data.UUID
import System.Random
#endif

-- |Initial connection with the server
-- Creates a 'Player' for that user given a name.
connect :: Server ConcurrentClientList -> Server ConcurrentChatList -> Name -> Server ()
connect remoteClientList remoteChats name = do
  chats <- remoteChats
  concurrentClientList <- remoteClientList
  sid <- getSessionID

  liftIO $ do
    CC.modifyMVar_ concurrentClientList  $ \clients -> do
      lobbyChannel <- CC.newChan
      return $ ClientEntry sid name [] lobbyChannel : clients

    clientList <- CC.readMVar concurrentClientList
    messageClients ClientJoined clientList

-- |Disconnect client from server.
disconnect :: LobbyState -> SessionID -> Server()
disconnect (clientList, games, chats) sid = do
  disconnectPlayerFromGame games clientList sid
  disconnectPlayerFromLobby clientList sid

  mVarClients <- clientList
  liftIO $ do
    clients <- CC.readMVar mVarClients
    messageClients ClientLeft clients

-- |Removes a player that has disconnected from player list
disconnectPlayerFromLobby :: Server ConcurrentClientList -> SessionID -> Server ()
disconnectPlayerFromLobby remoteClientList sid = do
  mVarClientList <- remoteClientList
  liftIO $ CC.modifyMVar_ mVarClientList $ \cs ->
    return $ filter ((sid /=) . sessionID) cs

-- |Removes a player that has disconnected from all games
disconnectPlayerFromGame :: Server GamesList -> Server ConcurrentClientList -> SessionID -> Server ()
disconnectPlayerFromGame remoteGames remoteClientList sid = do
  mVarGames <- remoteGames
  concurrentClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar concurrentClientList
  maybe
    (return ())
    (\client -> liftIO $ CC.modifyMVar_ mVarGames $ \games ->
      return $ map (deletePlayerFromGame (name client)) games)
    (lookupClientEntry sid clientList)

-- |Creates a new game on the server. The 'Int' represents the max number of players.
createGame :: Server GamesList -> Server ConcurrentClientList -> Int -> Server (Maybe String)
createGame remoteGames remoteClientList maxPlayers = do
  mVarClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar mVarClientList
  mVarGames <- remoteGames
  sid <- getSessionID
  gen <- liftIO newStdGen
  let (uuid, g) = random gen
  let uuidStr = Data.UUID.toString uuid

  liftIO $ maybe
    (return Nothing)
    (\c -> do
      CC.modifyMVar_ mVarGames $ \gs ->
        return $ (uuidStr, GameData [c] "GameName" maxPlayers) : gs
      messageClients GameAdded clientList
      return $ Just uuidStr)
    (lookupClientEntry sid clientList)


-- |Returns a list of the each game's uuid as a String
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  return $ getUUIDFromGamesList gameList

-- |Lets a player join a 'LobbyGame'. The 'String' represents the UUID for the game.
playerJoinGame :: Server ConcurrentClientList -> Server GamesList -> String -> Server Bool
playerJoinGame remoteClientList remoteGameList gameID = do
  clientList <- remoteClientList >>= liftIO . CC.readMVar
  mVarGamesList <- remoteGameList
  gameList <- liftIO $ CC.readMVar mVarGamesList
  sid <- getSessionID
  case findGameWithID gameID gameList of
    Nothing           -> return False
    Just (_,gameData) ->
      case (maxAmountOfPlayers gameData > length (players gameData), lookupClientEntry sid clientList) of
        (True, Just player) -> do
          liftIO $ do
            CC.modifyMVar_ mVarGamesList $
              \gList -> return $ addPlayerToGame player gameID gList
            messageClients PlayerJoinedGame (players gameData)
          return True
        _     -> return False

-- |Finds the name of a game given it's identifier
findGameNameWithID :: Server GamesList -> String -> Server String
findGameNameWithID remoteGames gid = do
  gamesList <- remoteGames >>= liftIO . CC.readMVar
  case findGameWithID gid gamesList of
    Just (_, gameData) -> return $ gameName gameData
    Nothing            -> return ""

-- |Finds the name of the game the client is currently in
findGameNameWithSid :: Server GamesList -> Server String
findGameNameWithSid remoteGames = do
  gamesList <- remoteGames >>= liftIO . CC.readMVar
  sid <- getSessionID
  case findGameWithSid sid gamesList of
    Just (_, gameData) -> return $ gameName gameData
    Nothing            -> return ""

-- |Finds the name of the players of the game the current client is in
playerNamesInGameWithSid :: Server GamesList -> Server [String]
playerNamesInGameWithSid remoteGames = do
  gamesList <- remoteGames >>= liftIO . CC.readMVar
  sid <- getSessionID
  case findGameWithSid sid gamesList of
    Nothing            -> return []
    Just (_, gameData) -> return $ map name (players gameData)

-- |Returns a list of strings containing all connected players names.
getConnectedPlayerNames :: Server ConcurrentClientList -> Server [String]
getConnectedPlayerNames remoteClientList = do
  concurrentClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar concurrentClientList
  return $ map name clientList

-- | Kicks the player with 'Name' from the game with id String
-- Currently does not notify the kicked person it has been kicked
-- Implement this if the method is used in the future.
kickPlayerWithGameID :: Server GamesList -> String -> Name -> Server ()
kickPlayerWithGameID remoteGames gameID clientName = do
  mVarGamesList <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGamesList $ \lst -> do
    let (h,t) = break ((gameID ==) . fst) lst
    case t of
      (game : gt)    -> return $ h ++ (deletePlayerFromGame clientName game) : gt
      _              -> return $ h ++ t

-- |Kicks the player with 'Name' from the game that the current client is in.
kickPlayerWithSid :: Server GamesList -> Name -> Server ()
kickPlayerWithSid remoteGames clientName = do
  mVarGamesList <- remoteGames
  gamesList <- liftIO $ CC.readMVar mVarGamesList
  sid <- getSessionID
  case findGameWithSid sid gamesList of
    Nothing   -> return ()
    Just game@(_,gameData) -> do
      liftIO $ CC.modifyMVar_ mVarGamesList $ \games ->
        return $ updateListElem (deletePlayerFromGame clientName) (== game) games
      case findClient clientName (players gameData) of
        Just c  -> liftIO $ messageClients KickedFromGame [c]
        Nothing -> return ()

-- |Change the nick name of the current player to that given.
changeNickName :: Server ConcurrentClientList -> Server GamesList -> Name -> Server ()
changeNickName remoteClientList remoteGames newName = do
  mVarClientList <- remoteClientList
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ mVarClientList $ \cs ->
    return $ updateNick sid cs
  mVarGamesList <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGamesList $ \gs ->
    return $ map (\(uuid, gameData) -> (uuid, gameData {players = updateNick sid $ players gameData})) gs

  -- Update the clients with this new information
  liftIO $ do
    clients <- CC.readMVar mVarClientList
    messageClients NickChange clients
  where
    updateNick sid = updateListElem (\c -> c {name = newName}) (\c -> sid == sessionID c)

-- |Change the name of a 'LobbyGame' given the game's ID
changeGameNameWithID :: Server GamesList -> Server ConcurrentClientList -> String -> Name -> Server ()
changeGameNameWithID remoteGames remoteClients uuid newName = do
  mVarGamesList <- remoteGames
  mVarClientList <- remoteClients
  gamesList <- liftIO $ CC.readMVar mVarGamesList
  case findGameWithID uuid gamesList of
    Nothing                -> return ()
    Just game@(_,gameData) ->
      liftIO $ CC.modifyMVar_ mVarGamesList $ \games ->
        return $ updateListElem
          (\(guuid, gData) -> (guuid, gData {gameName = newName}))
          (== game)
          games
  liftIO $ do
    clientsList <- CC.readMVar mVarClientList
    messageClients GameNameChange clientsList

-- |Change the name of a 'LobbyGame' that the connected client is in
changeGameNameWithSid :: Server GamesList -> Server ConcurrentClientList -> Name -> Server ()
changeGameNameWithSid remoteGames remoteClients newName = do
  mVarClientList <- remoteClients
  mVarGamesList <- remoteGames
  gamesList <- liftIO $ CC.readMVar mVarGamesList
  sid <- getSessionID
  case findGameWithSid sid gamesList of
    Nothing           -> return ()
    Just game@(_,gameData) ->
      liftIO $ CC.modifyMVar_ mVarGamesList $ \games ->
        return $ updateListElem
          (\(guuid, gData) -> (guuid, gData {gameName = newName}))
          (== game)
          games
  liftIO $ do
    clientsList <- CC.readMVar mVarClientList
    messageClients GameNameChange clientsList

-- |Reads the lobby channel of the current client and returns the message.
-- |Blocking method if the channel is empty
readLobbyChannel :: Server ConcurrentClientList -> Server LobbyMessage
readLobbyChannel remoteClientList = do
  mVarClientList <- remoteClientList
  sid <- getSessionID
  liftIO $ do
    clients <- CC.readMVar mVarClientList
    case find (\c -> sessionID c == sid) clients of
      Just client -> CC.readChan $ lobbyChannel client
      Nothing     -> error "readLobbyChannel: Could not find session ID"


-- |Changes the maximum number of players for a game
-- Requires that the player is the last in the player list (i.e. the owner)
changeMaxNumberOfPlayers :: Server GamesList -> Int -> Server ()
changeMaxNumberOfPlayers remoteGames newMax = do
  mVarGamesList <- remoteGames
  gamesList <- liftIO $ CC.readMVar mVarGamesList
  sid <- getSessionID
  if isOwnerOfGame sid gamesList then do
    case findGameWithSid sid gamesList of
      Nothing   -> return ()
      Just game ->
        liftIO $ CC.modifyMVar_ mVarGamesList $ \games ->
          return $ updateListElem
            (\(guuid, gData) -> (guuid, gData {maxAmountOfPlayers = newMax}))
            (== game)
            games
  else
    return ()

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
  concurrentClientList <- remoteClientList
  concurrentChatList <- remoteChatList

  cs <- liftIO $ CC.readMVar concurrentChatList
  if isNothing $ chatName `lookup` cs
    then do
      newChatChannel <- liftIO CC.newChan
      liftIO $ CC.modifyMVar_ concurrentChatList $ \chatList ->
        return $ (chatName, newChatChannel) : chatList
      addChannelToClient sid concurrentChatList concurrentClientList
    else
      addChannelToClient sid concurrentChatList concurrentClientList


    where
      addChannelToClient sid concurrentChatList concurrentClientList = do
        cs <- liftIO $ CC.readMVar concurrentChatList
        let chan = fromJust $ chatName `lookup` cs
        clientChan <- liftIO $ CC.dupChan chan
        let newChat = (chatName, clientChan)
        liftIO $ CC.modifyMVar_ concurrentClientList $ \clients -> do
          return $ updateListElem (addChatToClient newChat) ((sid ==) . sessionID) clients

      addChatToClient :: Chat -> ClientEntry -> ClientEntry
      addChatToClient chat client | chatName `elem` map fst (chats client) = client
                       | otherwise = client {chats = chat : chats client}

-- |Called by a client to read its various chat channels
readChatChannel :: Server ConcurrentClientList -> String ->  Server ChatMessage
readChatChannel remoteClientList chatName = do
  sid <- getSessionID
  concurrentClientList <- remoteClientList
  clients <- liftIO $ CC.readMVar concurrentClientList
  case sid `lookupClientEntry` clients of
    Nothing     -> return $ ChatError "Couldn't find clients sessionID in remotes client list, try reconnecting."
    Just client -> do
      case chatName `lookup` (chats client) of
        Nothing          -> return $ ChatError "Couldn't find chat in clients currently joined chats. join chat before trying to read from it"
        Just chatChannel -> liftIO $ CC.readChan chatChannel

-- | Called by the client to send a chat message
sendChatMessage :: Server ConcurrentClientList -> Server ConcurrentChatList -> String -> ChatMessage -> Server ()
sendChatMessage remoteClientList remoteChatList chatName chatMessage = do
  sid <- getSessionID
  concurrentClientList <- remoteClientList
  concurrentChatList <- remoteChatList
  clientList <- liftIO $ CC.readMVar concurrentClientList
  chatList <- liftIO $ CC.readMVar concurrentChatList

  case chatName `lookup` chatList of
    Nothing   -> return ()
    Just chatChannel -> do
      case sid `lookupClientEntry` clientList of
        Nothing     -> return()
        Just client -> do
          let msg = chatMessage {from = name client}
          liftIO $ CC.writeChan chatChannel $ mapMessage msg client

  return ()
    where
      mapMessage msg1 client = case msg1 of
        (ChatMessage from content) -> msg1
        ChatJoin                   -> ChatAnnounceJoin  (name client)
        ChatLeave                  -> ChatAnnounceLeave (name client)

-- |Called by a client to get its name based on sessionID
getClientName :: Server ConcurrentClientList -> Server String
getClientName remoteClientList = do
  sid <- getSessionID
  concurrentClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar concurrentClientList
  let client = find ((sid ==) . sessionID) clientList
  return $ name $ fromJust client
