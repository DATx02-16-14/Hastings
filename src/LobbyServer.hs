
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
  readLobbyChannel) where

import Haste.App
import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe
import LobbyTypes
import Chat
import Hastings.Utils
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
      chatChannel <- CC.newChan
      lobbyChannel <- CC.newChan
      return $ ClientEntry sid name chatChannel lobbyChannel : clients

    CC.modifyMVar_ chats $ \cs ->
      return $ addPlayerToChat sid "main" cs

    clientList <- CC.readMVar concurrentClientList
    messageClients ClientJoined clientList



-- |Disconnect client from server.
disconnect :: LobbyState -> SessionID -> Server()
disconnect (clientList, games, chats) sid = do
  disconnectPlayerFromChats chats sid
  disconnectPlayerFromGame games clientList sid
  disconnectPlayerFromLobby clientList sid

  mVarClients <- clientList
  liftIO $ do
    clients <- CC.readMVar mVarClients
    messageClients ClientLeft clients

disconnectPlayerFromChats :: Server ConcurrentChatList -> SessionID -> Server()
disconnectPlayerFromChats remoteChats sid = do
  chats <- remoteChats
  liftIO $ CC.modifyMVar_ chats $ \cs ->
    return $ removePlayerFromChats sid cs

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
  case lookupClientEntry sid clientList of
    Nothing -> return ()
    Just clientEntry -> liftIO $ CC.modifyMVar_ mVarGames $ \games -> mapM removePlayer games
      where
        removePlayer (uuid, GameData ps gameName) =
          let newClientList = filter ((name clientEntry /=) . name) ps in
          return (uuid, GameData newClientList gameName)

-- |Creates a new game on the server
createGame :: Server GamesList -> Server ConcurrentClientList -> Server (Maybe String)
createGame remoteGames remoteClientList = do
  concurrentClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar concurrentClientList
  games <- remoteGames
  sid <- getSessionID
  gen <- liftIO newStdGen
  let maybeClientEntry = lookupClientEntry sid clientList
  let (uuid, g) = random gen
  let uuidStr = Data.UUID.toString uuid

  liftIO $ CC.modifyMVar_ games $ \gs ->
    case maybeClientEntry of
        Just c  -> return $ (uuidStr,GameData [c] "GameName") : gs
        Nothing -> return gs
  liftIO $ messageClients GameAdded clientList
  case maybeClientEntry of
    Just p  -> return $ Just (uuidStr)
    Nothing -> return Nothing

-- |Returns a list of the each game's uuid as a String
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  return $ map fst gameList

-- |Lets a player join a 'LobbyGame'
playerJoinGame :: Server ConcurrentClientList -> Server GamesList -> String -> Server ()
playerJoinGame remoteClientList remoteGameList gameID = do
  clientList <- remoteClientList >>= liftIO . CC.readMVar
  gameList <- remoteGameList
  sid <- getSessionID
  case lookupClientEntry sid clientList of
    Just player -> liftIO $ CC.modifyMVar_ gameList $
      \gList -> return $ addPlayerToGame player gameID gList

    _ -> return ()

  maybeGame <- findGameWithSid gameList
  case maybeGame of
    Just (_,gameData) -> liftIO $ messageClients PlayerJoinedGame (players gameData)
    nothing           -> return ()

-- |Adds a player to a lobby game with the game ID
addPlayerToGame :: ClientEntry -> String -> [LobbyGame] -> [LobbyGame]
addPlayerToGame client gameID =
  updateListElem (\(gID, GameData ps gameName) -> (gID, GameData (nub $ client:ps) gameName)) (\g -> gameID == fst g)

-- |Finds the name of a game given it's identifier
findGameNameWithID :: Server GamesList -> String -> Server String
findGameNameWithID remoteGames gid = do
  mVarGamesList <- remoteGames
  maybeGame <- liftIO $ findGameWithID gid mVarGamesList
  case maybeGame of
    Just (_, GameData _ name) -> return name
    Nothing                   -> return ""

-- |Finds the name of the game the client is currently in
findGameNameWithSid :: Server GamesList -> Server String
findGameNameWithSid remoteGames = do
  mVarGamesList <- remoteGames
  maybeGame <- findGameWithSid mVarGamesList
  case maybeGame of
    Just (_, GameData _ name) -> return name
    Nothing                   -> return ""

-- |Finds the name of the players of a game given it's identifier
playerNamesInGameWithID :: Server GamesList -> String -> Server [String]
playerNamesInGameWithID remoteGameList gid = do
  mVarGamesList <- remoteGameList
  maybeGame <- liftIO $ findGameWithID gid mVarGamesList
  case maybeGame of
    Just (gid, GameData ps gameName)   -> return $ map name ps
    Nothing                            -> return []

-- |Finds the name of the players of the game the current client is in
playerNamesInGameWithSid :: Server GamesList -> Server [String]
playerNamesInGameWithSid remoteGameList = do
  mVarGamesList <- remoteGameList
  maybeGame <- findGameWithSid mVarGamesList
  case maybeGame of
    Nothing            -> return []
    Just (_, gameData) -> return $ map name (players gameData)

-- |Finds the 'LobbyGame' matching the first parameter and returns it
findGameWithID :: String -> GamesList -> IO (Maybe LobbyGame)
findGameWithID gid mVarGamesList = do
  gamesList <- CC.readMVar mVarGamesList
  return $ find (\g -> fst g == gid) gamesList

-- |Finds the 'LobbyGame' that the current connection is in (or the first if there are multiple)
findGameWithSid :: GamesList -> Server (Maybe LobbyGame)
findGameWithSid mVarGamesList = do
  gamesList <- liftIO $ CC.readMVar mVarGamesList
  sid <- getSessionID
  return $ find (\(_, gameData) -> sid `elem` sidsInGame gameData) gamesList
  where
    sidsInGame gameData = map sessionID $ players gameData

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
      ((_, GameData ps gameName):gt) -> return $ h ++ newGame:gt
        where
          newGame = (gameID, GameData (filter ((clientName /=) . name) ps) gameName)
      _              -> return $ h ++ t

-- |Kicks the player with 'Name' from the game that the current client is in.
kickPlayerWithSid :: Server GamesList -> Name -> Server ()
kickPlayerWithSid remoteGames clientName = do
  mVarGamesList <- remoteGames
  maybeGame <- findGameWithSid mVarGamesList
  case maybeGame of
    Nothing   -> return ()
    Just game@(_,gameData) -> do
      liftIO $ CC.modifyMVar_ mVarGamesList $ \games ->
        return $ updateListElem newGame (\g -> g == game) games
      case findClient clientName (players gameData) of
        Just c  -> liftIO $ messageClients KickedFromGame [c]
        Nothing -> return ()
      where
        newGame (guuid, GameData ps gameName) =
          (guuid, GameData (filter ((clientName /=) . name) ps) gameName)


-- |Change the nick name of the current player to that given.
changeNickName :: Server ConcurrentClientList -> Server GamesList -> Name -> Server ()
changeNickName remoteClientList remoteGames newName = do
  mVarClientList <- remoteClientList
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ mVarClientList $ \cs ->
    return $ updateNick sid cs
  mVarGamesList <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGamesList $ \gs ->
    return $ map (\(uuid, GameData ps gameName) -> (uuid, GameData (updateNick sid ps) gameName)) gs

  -- Update the clients with this new information
  liftIO $ do
    clients <- CC.readMVar mVarClientList
    messageClients NickChange clients
  where
    updateNick sid = updateListElem (\c -> c {name = newName}) (\c -> sid == sessionID c)

-- |Change the name of a 'LobbyGame' given the game's ID
changeGameNameWithID :: Server GamesList -> Server ConcurrentClientList -> String -> Name -> Server ()
changeGameNameWithID remoteGames remoteClients uuid newName = do
  gamesList <- remoteGames
  mVarClientList <- remoteClients
  maybeGame <- liftIO $ findGameWithID uuid gamesList
  case maybeGame of
    Nothing           -> return ()
    Just game@(_,gameData) -> liftIO $ do
      CC.modifyMVar_ gamesList $ \games ->
        return $ updateListElem
          (\(guuid, gData) -> (guuid, gData {gameName = newName}))
          (\g -> g == game)
          games
  liftIO $ do
    clientsList <- CC.readMVar mVarClientList
    messageClients GameNameChange (clientsList)

-- |Change the name of a 'LobbyGame' that the connected client is in
changeGameNameWithSid :: Server GamesList -> Server ConcurrentClientList -> Name -> Server ()
changeGameNameWithSid remoteGames remoteClients newName = do
  gamesList <- remoteGames
  mVarClientList <- remoteClients
  maybeGame <- findGameWithSid gamesList
  case maybeGame of
    Nothing           -> return ()
    Just game@(_,gameData) -> liftIO $ do
      CC.modifyMVar_ gamesList $ \games ->
        return $ updateListElem
          (\(guuid, gData) -> (guuid, gData {gameName = newName}))
          (\g -> g == game)
          games
  liftIO $ do
    clientsList <- CC.readMVar mVarClientList
    messageClients GameNameChange (clientsList)

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

-- |Finds the client with 'Name' from the list of 'ClientEntry'
findClient :: Name -> [ClientEntry] -> Maybe ClientEntry
findClient clientName clientList = find ((clientName ==).name) clientList

-- |Maps over the clients and writes the message to their channel
messageClients :: LobbyMessage -> [ClientEntry] -> IO ()
messageClients m cs = mapM_ (\c -> CC.writeChan (lobbyChannel c) m) cs
