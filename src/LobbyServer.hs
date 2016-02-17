module LobbyServer(handshake, closeConnection, createGame, getGamesList, playerJoinGame, LobbyGame)
  where
import Haste.App
import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe
import LobbyTypes

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
    liftIO $ CC.modifyMVar_ games $ \gs ->
        case maybePlayer of
            Just p -> return $ ("string",[p]) : gs
            Nothing -> return gs
    case maybePlayer of
        Just p -> return ("string", snd p)
        Nothing -> return ("false", "")

getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>=  liftIO . CC.readMVar
  return $ map fst gameList

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

addPlayerToGame :: Player -> String -> [LobbyGame] -> [LobbyGame]
addPlayerToGame plr gameID gameList =
  case find (\(ids, _) -> ids == gameID) gameList of
    Just (sessionID, gamePlayers) -> take n gameList ++ [(sessionID, plr:gamePlayers)] ++ drop (n + 1) gameList
    _ -> []

    where
      n = fromJust $ elemIndex gameID (map fst gameList)
