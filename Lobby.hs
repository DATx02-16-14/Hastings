module Lobby
  where
import Haste.App

import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe

type Name = String
type Player = (SessionID, Name)
type PlayerList = CC.MVar [Player]

type LobbyGame = (String, [Player])
type GamesList = CC.MVar [LobbyGame]

srvHandshake :: Server PlayerList -> Name -> Server ()
srvHandshake remotePlayers name = do
  players <- remotePlayers
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ players  $ \ps ->
    return $ (sid,name) : ps

srvCloseConnection :: Server PlayerList -> SessionID -> Server ()
srvCloseConnection remotePlayers sid = do
  players <- remotePlayers
  liftIO $ CC.modifyMVar_ players $ \ps ->
    return $ filter ((sid /=) . fst) ps

srvGetGamesList :: Server GamesList -> Server [String]
srvGetGamesList remoteGames = do
  gameList <- remoteGames >>=  liftIO . CC.readMVar
  return $ map fst gameList

srvPlayerJoinGame :: Server PlayerList -> Server GamesList -> String -> Server ()
srvPlayerJoinGame remotePlayers remoteGames gameID = do
  players <- remotePlayers >>= liftIO . CC.readMVar
  gameList <- remoteGames
  sid <- getSessionID
  case find (\(plrSid, _) -> plrSid == sid) players of
    Just plr -> liftIO $ CC.modifyMVar_ gameList $
      \gList -> return $ srvAddPlayerToGame plr gameID gList

    _ -> return ()

  return ()

srvAddPlayerToGame :: Player -> String -> [LobbyGame] -> [LobbyGame]
srvAddPlayerToGame plr gameID gameList =
  case find (\(ids, _) -> ids == gameID) gameList of
    Just (sessionID, gamePlayers) -> take n gameList ++ [(sessionID, plr:gamePlayers)] ++ drop (n + 1) gameList
    _ -> []

    where
      n = fromJust $ elemIndex gameID (map fst gameList)
