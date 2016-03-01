
{-# LANGUAGE CPP #-}
-- |Contains all functions that are meant to be run server side only. As such this only needs to be compiled with GHC and never with Haste.
module LobbyServer(
  connect,
  disconnect,
  createGame,
  getGamesList,
  playerJoinGame,
  playerNamesInGame,
  getConnectedPlayers,
  getConnectedPlayerNames,
  LobbyState) where

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
connect :: Server ClientMap -> Server ConcurrentChatList -> Name -> Server ()
connect remotePlayers remoteChats name = do
  chats <- remoteChats
  players <- remotePlayers
  sid <- getSessionID

  liftIO $ CC.modifyMVar_ players  $ \ps -> do
    --return $ (sid,name) : ps
    chatChannel <- CC.newChan
    return $ (sid, Player name chatChannel) : ps

  liftIO $ CC.modifyMVar_ chats $ \cs ->
    return $ addPlayerToChat sid "main" cs

-- |Disconnect client from server.
disconnect :: LobbyState -> SessionID -> Server()
disconnect (clientMap, games, chats) sid = do
  disconnectPlayerFromChats chats sid
  disconnectPlayerFromGame games clientMap sid
  disconnectPlayerFromLobby clientMap sid

disconnectPlayerFromChats :: Server ConcurrentChatList -> SessionID -> Server()
disconnectPlayerFromChats remoteChats sid = do
  chats <- remoteChats
  liftIO $ CC.modifyMVar_ chats $ \cs ->
    return $ removePlayerFromChats sid cs

-- |Removes a player that has disconnected from player list
disconnectPlayerFromLobby :: Server ClientMap -> SessionID -> Server ()
disconnectPlayerFromLobby remotePlayers sid = do
  players <- remotePlayers
  liftIO $ CC.modifyMVar_ players $ \ps ->
    return $ filter ((sid /=) . fst) ps

-- |Removes a player that has disconnected from all games
disconnectPlayerFromGame :: Server GamesList -> Server ClientMap -> SessionID -> Server ()
disconnectPlayerFromGame remoteGames remoteClientMap sid = do
  games <- remoteGames
  concurrentClientMap <- remoteClientMap
  clientMap <- liftIO $ CC.readMVar concurrentClientMap
  case lookup sid clientMap of
    Nothing -> return ()
    Just player -> do
      liftIO $ CC.modifyMVar_ games $ \games -> mapM removePlayer games
      where
        removePlayer game = do
          CC.modifyMVar_ game $ \(gName, players) ->
            return ( gName, filter (((name player) /=) . name) players )
          return game

-- |Creates a new game on the server
createGame :: Server GamesList -> Server ClientMap -> Server (String,String)
createGame remoteGames remotePlayers = do
  players <- remotePlayers
  games <- remoteGames
  sid <- getSessionID
  playerList <- liftIO $ CC.readMVar players
  let maybePlayer = lookup sid playerList
  gen <- liftIO newStdGen
  let (uuid, g) = random gen
  let uuidStr = Data.UUID.toString uuid
  liftIO $ CC.modifyMVar_ games $ \gs ->
    case maybePlayer of
        Just p -> do
          game <- liftIO $ CC.newMVar (uuidStr,[p])
          return $ game : gs
        Nothing -> return gs
  case maybePlayer of
    Just p -> return (uuidStr, name p)
    Nothing -> return ("false", "")

-- |Reteurns a list of the each game's name
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  liftIO $ mapM (\g -> do
    game <- CC.readMVar g
    return $ fst game) gameList

-- |Lets a player join a 'LobbyGame'
playerJoinGame :: Server ClientMap -> Server GamesList -> String -> Server ()
playerJoinGame remoteClientMap remoteGameList gameID = do
  clientMap <- remoteClientMap >>= liftIO . CC.readMVar
  gameList <- remoteGameList
  sid <- getSessionID
  case lookup sid clientMap of
    Just player -> liftIO $ CC.modifyMVar_ gameList $
      \gList -> addPlayerToGame player gameID gList

    _ -> return ()

  return ()

-- |Adds a player to a lobby game.
-- Is perhaps overly complicated since a LobbyGame is an MVar.
addPlayerToGame :: Player -> String -> [LobbyGame] -> IO [LobbyGame]
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

-- |Gets a list with all connected players
getConnectedPlayers :: Server ClientMap -> Server [Player]
getConnectedPlayers remoteClientMap = do
  clientMap <- remoteClientMap >>= liftIO . CC.readMVar
  return $ map snd clientMap

-- |Get
getConnectedPlayerNames :: Server ClientMap -> Server [String]
getConnectedPlayerNames remoteClientMap = do
  players <- getConnectedPlayers remoteClientMap
  return $ map name players
