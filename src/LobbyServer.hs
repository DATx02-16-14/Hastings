
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
  leaveGame,
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
  leaveChat,
  getClientName,
  getJoinedChats,
  getChats,
  setPasswordToGame,
  isGamePasswordProtected,
  remoteIsOwnerOfGame) where

import Haste.App

import qualified Control.Concurrent as CC
import Control.Monad (when)

import Data.List
import Data.Maybe
import Data.ByteString.Char8 (ByteString, empty, pack, unpack)

import Crypto.PasswordStore (makePassword, verifyPassword)

import LobbyTypes
import Hastings.Utils
import Hastings.ServerUtils

#ifndef __HASTE__
import Data.UUID
import System.Random
import qualified Hastings.Database.Player as PlayerDB
import qualified Hastings.Database.Fields as Fields
import Database.Persist (entityKey, entityVal, Entity)
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

    liftIO $ PlayerDB.saveOnlinePlayer name sid
    clientList <- CC.readMVar concurrentClientList
    messageClients ClientJoined clientList

-- |Disconnect client from server.
disconnect :: LobbyState -> SessionID -> Server()
disconnect (clientList, games, chats) sid = do
  cs <- clientList >>= liftIO . CC.readMVar
  maybe
    (return ())
    (\c -> do
      notifyClientChats clientList $ name c ++ " disconnected"
      return ())
    $ sid `lookupClientEntry` cs

  leaveGame games
  disconnectPlayerFromLobby clientList sid
  liftIO $ PlayerDB.deleteOnlinePlayer sid

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

-- |Removes a player that has disconnected from it's game
leaveGame :: Server GamesList -> Server ()
leaveGame remoteGames = do
  mVarGames <- remoteGames
  gamesList <- liftIO $ CC.readMVar mVarGames
  sid <- getSessionID
  case findGameWithSid sid gamesList of
    Nothing                 -> return ()
    Just game@(_, gameData) -> liftIO $ do
      CC.modifyMVar_ mVarGames $ \games ->
        return $ updateListElem (removePlayer sid) (== game) games
      maybe
        (return ())
        (\p -> messageClients KickedFromGame [p])
        (lookupClientEntry sid $ players gameData)
      messageClients PlayerLeftGame $ players gameData

  where
    removePlayer sid (uuid, gameData) =
      let newClientList = filter ((sid /=) . sessionID) $ players gameData in
      (uuid, gameData {players = newClientList})

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
        return $ (uuidStr, GameData [c] "GameName" maxPlayers empty) : gs
      messageClients GameAdded clientList
      return $ Just uuidStr)
    (lookupClientEntry sid clientList)

-- |Returns a list of the each game's uuid as a String
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  return $ getUUIDFromGamesList gameList

-- |Lets a player join a 'LobbyGame'. The 'String' represents the UUID for the game.
-- |The second 'String' is the password for the game, if there is no password it can be left empty.
playerJoinGame :: Server ConcurrentClientList -> Server GamesList -> String -> String -> Server Bool
playerJoinGame remoteClientList remoteGameList gameID passwordString = do
  clientList <- remoteClientList >>= liftIO . CC.readMVar
  mVarGamesList <- remoteGameList
  gameList <- liftIO $ CC.readMVar mVarGamesList
  sid <- getSessionID
  case (lookupClientEntry sid clientList, findGameWithID gameID gameList) of
    (Just player, Just (_,gameData)) -> do
      let passwordOfGame = gamePassword gameData
      if passwordOfGame == empty || verifyPassword (pack passwordString) passwordOfGame
        then if maxAmountOfPlayers gameData > length (players gameData)
          then liftIO $ do
            CC.modifyMVar_ mVarGamesList $
              \gList -> return $ addPlayerToGame player gameID gList
            messageClients PlayerJoinedGame (players gameData)
            return True
          else do
            liftIO $ messageClients (LobbyError "Game is full") [player]
            return False
        else do
          liftIO $ messageClients (LobbyError "Wrong password") [player]
          return False
    _                                -> return False


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

-- |Kicks the player with index 'Int' from the list of players in
-- the game that the current client is in.
kickPlayerWithSid :: Server GamesList
                  -> Int  -- ^The index in the list of players of the player to kick
                  -> Server ()
kickPlayerWithSid remoteGames clientIndex = do
  mVarGamesList <- remoteGames
  gamesList <- liftIO $ CC.readMVar mVarGamesList
  sid <- getSessionID
  case findGameWithSid sid gamesList of
    Nothing   -> return ()
    Just game@(_,gameData) ->
      liftIO $ do
        CC.modifyMVar_ mVarGamesList $ \games ->
          return $ updateListElem (deletePlayerFromGame clientIndex) (== game) games
        messageClients KickedFromGame [players gameData !! clientIndex]
        messageClients PlayerLeftGame $ players gameData

-- |Change the nick name of the current player to that given.
changeNickName :: Server ConcurrentClientList -> Server GamesList -> Name -> Server ()
changeNickName remoteClientList remoteGames newName = do
  mVarClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar mVarClientList
  sid <- getSessionID
  player <- liftIO $ PlayerDB.retrieveOnlinePlayer sid
  liftIO $ PlayerDB.changeUserName (oldName player) newName

  liftIO $ CC.modifyMVar_ mVarClientList $ \cs ->
    return $ updateNick sid cs
  mVarGamesList <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGamesList $ \gs ->
    return $ map (\(uuid, gameData) -> (uuid, gameData {players = updateNick sid $ players gameData})) gs

  -- Update the clients with this new information
  liftIO $ do
    clients <- CC.readMVar mVarClientList
    messageClients NickChange clients
  -- Notify all chats about nick update
  notifyClientChats remoteClientList $ oldName player ++ " changed nick to " ++ newName

  where
    updateNick sid = updateListElem (\c -> c {name = newName}) (\c -> sid == sessionID c)
    oldName = maybe "NO_SUCH_CLIENT" (Fields.playerUserName . entityVal)

-- | Sends a server notification to all chats the client has joined
notifyClientChats :: Server ConcurrentClientList -> String -> Server ()
notifyClientChats remoteClients notification = do
  clientList <- remoteClients >>= liftIO . CC.readMVar
  sid <- getSessionID
  liftIO . maybe
    (print "notifyClientChats > Could not find sid in connected clients")
    (mapM_ (flip CC.writeChan (ChatMessage "SERVER" notification) . snd) . chats)
    $ sid `lookupClientEntry` clientList


-- |Change the name of a 'LobbyGame' given the game's ID
changeGameNameWithID :: Server GamesList -> Server ConcurrentClientList -> String -> Name -> Server ()
changeGameNameWithID remoteGames remoteClients uuid newName = do
  mVarGamesList <- remoteGames
  gamesList <- liftIO $ CC.readMVar mVarGamesList
  mVarClientList <- remoteClients
  let maybeGame = findGameWithID uuid gamesList
  case maybeGame of
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
  when (isOwnerOfGame sid gamesList) $
    case findGameWithSid sid gamesList of
      Nothing   -> return ()
      Just game ->
        liftIO $ CC.modifyMVar_ mVarGamesList $ \games ->
          return $ updateListElem
            (\(guuid, gData) -> (guuid, gData {maxAmountOfPlayers = newMax}))
            (== game)
            games

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
      announceChatJoin remoteClientList remoteChatList chatName
    else do
      addChannelToClient sid concurrentChatList concurrentClientList
      announceChatJoin remoteClientList remoteChatList chatName


    where
      addChannelToClient sid concurrentChatList concurrentClientList = do
        cs <- liftIO $ CC.readMVar concurrentChatList
        let chan = fromJust $ chatName `lookup` cs
        clientChan <- liftIO $ CC.dupChan chan
        let newChat = (chatName, clientChan)
        liftIO $ CC.modifyMVar_ concurrentClientList $ \clients ->
          return $ updateListElem (addChatToClient newChat) ((sid ==) . sessionID) clients

      addChatToClient :: Chat -> ClientEntry -> ClientEntry
      addChatToClient chat client | chatName `elem` map fst (chats client) = client
                       | otherwise = client {chats = chat : chats client}

-- | Sends a ChatAnnounceJoin to all clients present in the channel.
-- | String is the name of the channel joined
announceChatJoin :: Server ConcurrentClientList -> Server ConcurrentChatList -> String -> Server ()
announceChatJoin remoteClientList remoteChatList chatName = do
  clientList <- remoteClientList >>= liftIO . CC.readMVar
  chatList <- remoteChatList >>= liftIO . CC.readMVar
  case chatName `lookup` chatList of
    Nothing      -> return ()
    Just channel -> do
      sid <- getSessionID
      maybe
        (return ())
        (liftIO . CC.writeChan channel . ChatAnnounceJoin . name)
        $ sid `lookupClientEntry` clientList

-- | Called by client to leave the named Chat
-- | String is the name of the chat to be left
leaveChat :: Server ConcurrentClientList -> String -> Server ()
leaveChat remoteClientList chatName = do
  sid <- getSessionID
  concurrentClientList <- remoteClientList
  cs <- liftIO $ CC.readMVar concurrentClientList
  case sid `lookupClientEntry` cs of
    Nothing     -> return ()
    Just client ->
      case chatName `lookup` chats client of
        Nothing      -> return ()
        Just channel -> do
          liftIO $ do
            CC.writeChan channel $ ChatAnnounceLeave $ name client
            CC.modifyMVar_ concurrentClientList $ \clientList ->
              return $ updateListElem (\c -> c {chats =
                deleteBy (\(cName1,_) (cName2,_) -> cName1 == cName2) (chatName, channel) $ chats client
                }) ((sessionID client ==) . sessionID) clientList
          return ()



-- |Called by a client to read its various chat channels
readChatChannel :: Server ConcurrentClientList -> String ->  Server ChatMessage
readChatChannel remoteClientList chatName = do
  sid <- getSessionID
  concurrentClientList <- remoteClientList
  clients <- liftIO $ CC.readMVar concurrentClientList
  case sid `lookupClientEntry` clients of
    Nothing     -> return $ ChatError "Couldn't find clients sessionID in remotes client list, try reconnecting."
    Just client ->
      case chatName `lookup` chats client of
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
    Just chatChannel ->
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
  return $ name $ fromJust $ lookupClientEntry sid clientList

-- | Return list of chatnames which the client have joined
getJoinedChats :: Server ConcurrentClientList -> Server [String]
getJoinedChats remoteClientList = do
  sid <- getSessionID
  clientList <- remoteClientList >>= liftIO . CC.readMVar
  case sid `lookupClientEntry` clientList of
    Nothing     -> do
      liftIO $ print "getJoinedChats > Error: expected client not found."
      return []
    Just client ->
      return $ map fst $ chats client

-- | Return list of all chatnames
getChats :: Server ConcurrentChatList -> Server [String]
getChats remoteChatList = do
  chatList <- remoteChatList >>= liftIO . CC.readMVar
  return $ map fst chatList

-- |Sets the password (as a 'ByteString') of the game the client is in.
-- |Only possible if the client is the owner of the game.
setPasswordToGame :: Server GamesList -> String -> Server ()
setPasswordToGame remoteGames passwordString = do
  let password = pack passwordString
  hashedPassword <- liftIO $ makePassword password 17
  mVarGames <- remoteGames
  gamesList <- remoteGames >>= liftIO . CC.readMVar
  sid <- getSessionID
  case (findGameWithSid sid gamesList, isOwnerOfGame sid gamesList) of
    (Just game, True)          -> liftIO $
      CC.modifyMVar_ mVarGames $ \games ->
        return $ updateListElem
          (\(guuid, gData) -> (guuid, gData {gamePassword = hashedPassword}))
          (== game)
          games
    (Just (_,gameData), False) -> do
      sid <- getSessionID
      maybe
        (return ())
        (\client -> liftIO $ messageClients (LobbyError "Not owner of the game") [client])
        (lookupClientEntry sid (players gameData))

-- |Returns True if game is password protected, False otherwise. 'String' is the UUID of the game
isGamePasswordProtected :: Server GamesList -> String -> Server Bool
isGamePasswordProtected remoteGames guuid = do
  gamesList <- remoteGames >>= liftIO . CC.readMVar
  case findGameWithID guuid gamesList of
    Nothing           -> return False
    Just (_,gameData) -> return $ gamePassword gameData /= empty
