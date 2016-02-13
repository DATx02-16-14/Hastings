module Lobby
  where
import Haste.App
import Data.UUID

import qualified Control.Concurrent as CC

type Name = String
type Player = (SessionID, Name)
type PlayerList = CC.MVar [Player]

type LobbyGame = (UUID, [Player])
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
