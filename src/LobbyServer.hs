
{-# LANGUAGE CPP #-}
-- |Contains all functions that are meant to be run server side only. As such this only needs to be compiled with GHC and never with Haste.
module LobbyServer(
  handshake,
  createGame,
  getGamesList,
  playerJoinGame,
  playerNamesInGame,
  getConnectedPlayers,
  disconnectPlayerFromLobby,
  disconnectPlayerFromGame,
  kickPlayer,
  changeNickName) where

import Haste.App
import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe
import LobbyTypes
import Hastings.Utils
#ifndef __HASTE__
import Data.UUID
import System.Random
#endif

-- |Initial handshake with the server
-- Creates a 'Player' for that user given a name.
handshake :: Server PlayerList -> Name -> Server ()
handshake remotePlayers name = do
  players <- remotePlayers
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ players  $ \ps ->
    return $ (sid,name) : ps

-- |Removes a player that has disconnected from player list
disconnectPlayerFromLobby :: Server PlayerList -> SessionID -> Server ()
disconnectPlayerFromLobby remotePlayers sid = do
  players <- remotePlayers
  liftIO $ CC.modifyMVar_ players $ \ps ->
    return $ filter ((sid /=) . fst) ps

-- |Removes a player that has disconnected from all games
disconnectPlayerFromGame :: Server GamesList -> SessionID -> Server ()
disconnectPlayerFromGame remoteGames sid = do
  games <- remoteGames
  liftIO $ CC.modifyMVar_ games $ \game -> mapM removePlayer game
  where
    removePlayer game = do
      CC.modifyMVar_ game $ \(str, players) ->
        return (str, filter ((sid /=) . fst) players)
      return game

-- |Creates a new game on the server
createGame :: Server GamesList -> Server PlayerList -> Server (Maybe (String,String))
createGame remoteGames remotePlayers = do
  players <- remotePlayers
  games <- remoteGames
  sid <- getSessionID
  playerList <- liftIO $ CC.readMVar players
  let maybePlayer = find (\p -> fst p == sid) playerList
  gen <- liftIO newStdGen
  let (uuid, g) = random gen
  let uuidStr = Data.UUID.toString uuid
  liftIO $ CC.modifyMVar_ games $ \gs ->
    case maybePlayer of
        Just p  -> do
          game <- liftIO $ CC.newMVar (uuidStr,[p])
          return $ game : gs
        Nothing -> return gs
  case maybePlayer of
    Just p  -> return $ Just (uuidStr, snd p)
    Nothing -> return Nothing

-- |Reteurns a list of the each game's name
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  liftIO $ mapM (\g -> do
    game <- CC.readMVar g
    return $ fst game) gameList

-- |Lets a player join a 'LobbyGame'
playerJoinGame :: Server PlayerList -> Server GamesList -> String -> Server ()
playerJoinGame remotePlayers remoteGames gameID = do
  players <- remotePlayers >>= liftIO . CC.readMVar
  gameList <- remoteGames
  sid <- getSessionID
  case find (\(plrSid, _) -> plrSid == sid) players of
    Just plr -> liftIO $ CC.modifyMVar_ gameList $
      \gList -> addPlayerToGame plr gameID gList

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
playerNamesInGame remoteGames gid = do
  mVarGamesList <- remoteGames
  game <- liftIO $ findGame gid mVarGamesList
  case game of
    Just mVarG -> do
      gam <- liftIO $ CC.readMVar mVarG
      return $ map snd $ snd gam
    Nothing    -> return []

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
getConnectedPlayers :: Server PlayerList -> Server [String]
getConnectedPlayers remotePlayers = do
  playerList <- remotePlayers >>= liftIO . CC.readMVar
  return $ map snd playerList

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
          let list = filter (\p -> playerName /= snd p) $ snd g
          return (gameID, list)
        return $ gh ++ game:gt
      Nothing -> return $ gh ++ gt

changeNickName :: Server PlayerList -> Server GamesList -> Name -> Server ()
changeNickName remotePlayers remoteGames newName = do
  mVarPlayers <- remotePlayers
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ mVarPlayers $ \ps ->
    return $ updateList ps sid newName
  mVarGamesList <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGamesList $ \gs ->
    updateListMVar gs sid newName
  where
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

    updateList :: [Player] -> SessionID -> Name -> [Player]
    updateList ps sid newName = psHead ++ (sid,newName):psTail
      where
        (psHead,p:psTail) = span (\p -> sid /= fst p) ps
