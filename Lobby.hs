module Lobby
  where
import Haste.App
import qualified Control.Concurrent as CC
import Data.List
import Haste.DOM

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

srvCreateGame :: Server GamesList -> Server PlayerList -> Server (String,String)
srvCreateGame remoteGames remotePlayers = do
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


createLobbyDOM :: IO ()
createLobbyDOM = do
   createGamebtn <- newElem "button" `with`
      [
         prop "id" =: "createGamebtn"
      ]
   crGamebtnText <- newTextElem "Create new game"

   appendChild createGamebtn crGamebtnText
   appendChild documentBody createGamebtn

deleteLobbyDOM :: IO ()
deleteLobbyDOM =
   withElems ["createGamebtn"] $ \[createGamebtn] ->
      deleteChild documentBody createGamebtn