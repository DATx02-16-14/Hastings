
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
  mVarGames <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGames $ \games -> return $ map removePlayer games
  where
    removePlayer (str, players) = (str, filter ((sid /=) . fst) players)

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
          return $ (uuidStr,[p]) : gs
        Nothing -> return gs
  case maybePlayer of
    Just p  -> return $ Just (uuidStr, snd p)
    Nothing -> return Nothing

-- |Reteurns a list of the each game's name
getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  return $ map (fst) gameList

-- |Lets a player join a 'LobbyGame'
playerJoinGame :: Server PlayerList -> Server GamesList -> String -> Server ()
playerJoinGame remotePlayers remoteGames gameID = do
  players <- remotePlayers >>= liftIO . CC.readMVar
  gameList <- remoteGames
  sid <- getSessionID
  case find (\(plrSid, _) -> plrSid == sid) players of
    Just plr -> liftIO $ CC.modifyMVar_ gameList $
      \gList -> return $ addPlayerToGame plr gameID gList

    _ -> return ()

  return ()

-- |Adds a player to a lobby game.
-- Is perhaps overly complicated since a LobbyGame is an MVar.
addPlayerToGame :: Player -> String -> [LobbyGame] -> [LobbyGame]
addPlayerToGame plr gameID gamesList =
  case tg of
    (g:t) -> hg ++ (fst g, plr:snd g):tg
    _     -> error "No such game"
  where
    (hg,tg) = span (\g -> gameID /= fst g) gamesList

-- |Finds the name of a game given it's identifier
-- (seems useless since the name is the identifier atm.)
findGameName :: Server GamesList -> String -> Server String
findGameName remoteGames gid = do
  mVarGamesList <- remoteGames
  maybeGame <- liftIO $ findGame gid mVarGamesList
  case maybeGame of
    Just game -> return $ fst game
    Nothing   -> return ""

-- |Finds the name of the players of a game given it's identifier
playerNamesInGame :: Server GamesList -> String -> Server [String]
playerNamesInGame remoteGames gid = do
  mVarGamesList <- remoteGames
  maybeGame <- liftIO $ findGame gid mVarGamesList
  case maybeGame of
    Just game  -> return $ map snd $ snd game
    Nothing    -> return []

-- |Finds the 'LobbyGame' matching the first parameter and returns it
findGame :: String -> GamesList -> IO (Maybe LobbyGame)
findGame gid mVarGamesList = do
  gamesList <- CC.readMVar mVarGamesList
  return $ find (\g -> fst g == gid) gamesList

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
    let (h,t) = span (\g -> gameID /= fst g) lst
    case t of
      ((_,players):gt) -> return $ h ++ newGame:gt
        where
          newGame = (gameID, filter (\p -> playerName /= snd p) players)
      _              -> return $ h ++ t

-- |Change the nick name of the current player to that given.
changeNickName :: Server PlayerList -> Server GamesList -> Name -> Server ()
changeNickName remotePlayers remoteGames newName = do
  mVarPlayers <- remotePlayers
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ mVarPlayers $ \ps ->
    return $ updatePlayerList ps sid newName
  mVarGamesList <- remoteGames
  liftIO $ CC.modifyMVar_ mVarGamesList $ \gs ->
    return $ map (updateGame sid newName) gs
  where
    updateGame :: SessionID -> Name -> LobbyGame -> LobbyGame
    updateGame sid newName (gid, ps) = (gid, updatePlayerList ps sid newName)
    -- Helper method to update a list
    updatePlayerList :: [Player] -> SessionID -> Name -> [Player]
    updatePlayerList ps sid newName = psHead ++ (sid,newName):psTail
      where
        (psHead,p:psTail) = span (\p -> sid /= fst p) ps
