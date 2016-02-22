<<<<<<< HEAD
module LobbyServer(handshake, closeConnection, createGame, getGamesList, playerJoinGame, playerNamesInGame, getConnectedPlayers)
=======
{-# LANGUAGE CPP #-}
module LobbyServer(handshake, closeConnection, createGame, getGamesList, playerJoinGame, playerNamesInGame)
>>>>>>> 8e05825... Add uuid to creation of a game
  where
import Haste.App
import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe
import LobbyTypes
import Hastings.Utils
#ifdef __HASTE__
#else
import Data.UUID
import System.Random
#endif

handshake :: Server PlayerList -> Name -> Server ()
handshake remotePlayers name = do
  players <- remotePlayers
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ players  $ \ps ->
    return $ (sid,name) : ps

closeConnection :: Server PlayerList -> SessionID -> Server ()
closeConnection remotePlayers sid = do
  players <- remotePlayers
  liftIO $ CC.modifyMVar_ players $ \ps ->
    return $ filter ((sid /=) . fst) ps

createGame :: Server GamesList -> Server PlayerList -> Server (String,String)
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
        Just p -> do
          game <- liftIO $ CC.newMVar (uuidStr,[p])
          return $ game : gs
        Nothing -> return gs
  case maybePlayer of
    Just p -> return (uuidStr, snd p)
    Nothing -> return ("false", "")

getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  liftIO $ mapM (\g -> do
    game <- CC.readMVar g
    return $ fst game) gameList

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

-- Adds a player to a lobby game.
-- Is perhaps overly complicated since a LobbyGame is an MVar.
addPlayerToGame :: Player -> String -> [LobbyGame] -> IO [LobbyGame]
addPlayerToGame plr gameID gameList = do
  ga <- findIO (\game -> do
                 g <- CC.readMVar game
                 return $ (fst g) == gameID) gameList
  case ga of
    (Just mVarGame, hs, ts) -> do
      modGame <- CC.modifyMVar mVarGame $
        \g -> do
          let (sessionID, gamePlayers) = g
          return $ ((sessionID, plr:gamePlayers), (sessionID, plr:gamePlayers))
      g <- CC.newMVar modGame
      return $ hs ++ (g:ts)
    (Nothing, _, _) -> error "addPlayerToGame: Could not add player"

-- Finds the name of a game given it's identifier
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

-- Finds the name of the players of a game given it's identifier
playerNamesInGame :: Server GamesList -> String -> Server [String]
playerNamesInGame remoteGames gid = do
  mVarGamesList <- remoteGames
  game <- liftIO $ findGame gid mVarGamesList
  case game of
    Just mVarG -> do
      gam <- liftIO $ CC.readMVar mVarG
      return $ map snd $ snd gam
    Nothing    -> return []


-- Finds the game matching the first parameter and returns it
findGame :: String -> GamesList -> IO (Maybe LobbyGame)
findGame gid mVarGamesList = do
  gamesList <- CC.readMVar mVarGamesList
  mVarGame <- findIO (\game -> do
                 g <- CC.readMVar game
                 return $ (fst g) == gid) gamesList
  let (ga,_,_) = mVarGame
  return ga

getConnectedPlayers :: Server PlayerList -> Server [String]
getConnectedPlayers remotePlayers = do
  playerList <- remotePlayers >>= liftIO . CC.readMVar
  return $ map snd playerList
