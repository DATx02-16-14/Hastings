
{-# LANGUAGE CPP #-}
-- |Contains all functions that are meant to be run server side only. As such this only needs to be compiled with GHC and never with Haste.
module LobbyServer(
  connect,
  disconnect,
  createGame,
  getGamesList,
  playerJoinGame,
  playerNamesInGame,
  getConnectedPlayerNames,
  disconnectPlayerFromLobby,
  disconnectPlayerFromGame,
  kickPlayer,
  changeNickName) where

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

  liftIO $ CC.modifyMVar_ concurrentClientList  $ \clients -> do
    chatChannel <- CC.newChan
    return $ (ClientEntry sid name chatChannel) : clients

  liftIO $ CC.modifyMVar_ chats $ \cs ->
    return $ addPlayerToChat sid "main" cs

-- |Disconnect client from server.
disconnect :: LobbyState -> SessionID -> Server()
disconnect (clientList, games, chats) sid = do
  disconnectPlayerFromChats chats sid
  disconnectPlayerFromGame games clientList sid
  disconnectPlayerFromLobby clientList sid

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
  games <- remoteGames
  concurrentClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar concurrentClientList
  case lookupClientEntry sid clientList of
    Nothing -> return ()
    Just clientEntry -> do
      liftIO $ CC.modifyMVar_ games $ \games -> mapM removePlayer games
      where
        removePlayer game = do
          CC.modifyMVar_ game $ \(gName, players) ->
            return ( gName, filter (((name clientEntry) /=) . name) players )
          return game

-- |Creates a new game on the server
createGame :: Server GamesList -> Server ConcurrentClientList -> Server (Maybe (String,String))
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
        Just p  -> do
          game <- liftIO $ CC.newMVar (uuidStr,[p])
          return $ game : gs
        Nothing -> return gs
  case maybeClientEntry of
    Just p  -> return $ Just (uuidStr, name p)
    Nothing -> return Nothing

-- |Reteurns a list of the each game's name
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  liftIO $ mapM (\g -> do
    game <- CC.readMVar g
    return $ fst game) gameList

-- |Lets a player join a 'LobbyGame'
playerJoinGame :: Server ConcurrentClientList -> Server GamesList -> String -> Server ()
playerJoinGame remoteClientList remoteGameList gameID = do
  clientList <- remoteClientList >>= liftIO . CC.readMVar
  gameList <- remoteGameList
  sid <- getSessionID
  case lookupClientEntry sid clientList of
    Just player -> liftIO $ CC.modifyMVar_ gameList $
      \gList -> addPlayerToGame player gameID gList

    _ -> return ()

  return ()

-- |Adds a player to a lobby game.
-- Is perhaps overly complicated since a LobbyGame is an MVar.
addPlayerToGame :: ClientEntry -> String -> [LobbyGame] -> IO [LobbyGame]
addPlayerToGame plr gameID gameList = do
  ga <- findIO (\game -> do
                 g <- CC.readMVar game
                 return $ fst g == gameID) gameList
  case ga of
    (Just mVarGame, hs, ts) -> do
      modGame <- CC.modifyMVar mVarGame $
        \g -> do
          let (sessionID, gamePlayers) = g
          return ((sessionID, plr:gamePlayers), (sessionID, plr:gamePlayers))
      g <- CC.newMVar modGame
      return $ hs ++ (g:ts)
    (Nothing, _, _) -> error "addPlayerToGame: Could not add player"

-- |Finds the name of a game given it's identifier
-- (seems useless since the name is the identifier atm.)
findGameName :: Server GamesList -> String -> Server String
findGameName remoteGames gid = do
  mVarGamesList <- remoteGames
  game <- liftIO $ findGame gid mVarGamesList
  case game of
    Just mVarG -> do
      gam <- liftIO $ CC.readMVar mVarG
      return $ fst gam
    Nothing    -> return ""

-- |Finds the name of the players of a game given it's identifier
playerNamesInGame :: Server GamesList -> String -> Server [String]
playerNamesInGame remoteGameList gid = do
  concurrentGameList <- remoteGameList
  game <- liftIO $ findGame gid concurrentGameList
  case game of
    Nothing    -> return []
    Just mVarG -> do
      game <- liftIO $ CC.readMVar mVarG
      return $ map name $ snd game

-- |Finds the 'LobbyGame' matching the first parameter and returns it
findGame :: String -> GamesList -> IO (Maybe LobbyGame)
findGame gid mVarGamesList = do
  gamesList <- CC.readMVar mVarGamesList
  mVarGame <- findIO (\game -> do
                 g <- CC.readMVar game
                 return $ fst g == gid) gamesList
  let (ga,_,_) = mVarGame
  return ga

-- |Returns a list of strings containing all connected players names.
getConnectedPlayerNames :: Server ConcurrentClientList -> Server [String]
getConnectedPlayerNames remoteClientList = do
  concurrentClientList <- remoteClientList
  clientList <- liftIO $ CC.readMVar concurrentClientList
  return $ map name clientList

-- | Kicks the player with 'Name' from the game with id String
kickPlayer :: Server GamesList -> String -> Name -> Server ()
kickPlayer remoteGames gameID playerName = do
  mVarGamesList <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGamesList $ \lst -> do
    (maybeGame, gh, gt) <- findIO
      (\game -> do
         g <- CC.readMVar game
         return $ fst g == gameID) lst
    case maybeGame of
      Just game -> do
        liftIO $ CC.modifyMVar_ game $ \g -> do
          let list = filter (\p -> playerName /= name p) $ snd g
          return (gameID, list)
        return $ gh ++ game:gt
      Nothing -> return $ gh ++ gt

-- |Change the nick name of the current player to that given.
changeNickName :: Server ConcurrentClientList -> Server GamesList -> Name -> Server ()
changeNickName remoteClientList remoteGames newName = do
  mVarClientList <- remoteClientList
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ mVarClientList $ \cs -> do
    return $ updateListElem (\c -> c {name = newName}) (\c -> sid == (sessionID c)) cs
  {- Commented out since merge from development to chat.
  mVarGamesList <- remoteGames
  -liftIO $ CC.modifyMVar_ mVarGamesList $ \gs ->
  -  updateListMVar gs sid newName
  where
    -- Helper method to update a list with MVars
    updateListMVar :: [LobbyGame] -> SessionID -> Name -> IO [LobbyGame]
    updateListMVar gs sid newName = do
      updateListMVar' gs
      return gs
        where
          updateListMVar' :: [LobbyGame] -> IO ()
          updateListMVar' [] = return ()
          updateListMVar' (g:gs) = do
            CC.modifyMVar_ g $ \(gid, ps) ->
              return (gid, updateList ps sid newName)
            updateListMVar' gs
  -}
