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
   parentDiv <- newElem "div" `with`
      [
         prop "id" =: "lobby"
      ]
   createGamebtn <- newElem "button" `with`
      [
         prop "id" =: "createGamebtn"
      ]
   crGamebtnText <- newTextElem "Create new game"

   appendChild createGamebtn crGamebtnText
   appendChild parentDiv createGamebtn
   appendChild documentBody parentDiv

createGameDOM :: (String,[String]) -> IO ()
createGameDOM (gameId,ps) = do
    parentDiv <- newElem "div" `with`
        [
            prop "id" =: "lobbyGame"
        ]
    nameOfGame <- newTextElem gameId
    header <- newElem "h1" `with`
        [
            style "text-align" =: "center",
            style "margin-left" =: "auto",
            style "margin-right" =: "auto"
        ]
    appendChild header nameOfGame
    appendChild parentDiv header
    list <- newElem "div" `with`
        [
            prop "id" =: "playerList"
        ]
    listhead <- newTextElem "Players: "
    appendChild list listhead
    mapM_ (\p -> do
                name <- newTextElem $ p ++ " "
                appendChild list name
            ) ps
    appendChild parentDiv list
    appendChild documentBody parentDiv

createGameDOMWithGame :: LobbyGame -> IO ()
createGameDOMWithGame lobbyGame = createGameDOM (fst lobbyGame, map snd $ snd lobbyGame)

deleteLobbyDOM :: IO ()
deleteLobbyDOM = deleteDOM "lobby"

deleteGameDOM :: IO ()
deleteGameDOM = deleteDOM "lobbyGame"

deleteDOM :: String -> IO ()
deleteDOM s = withElems [s] $ \[element] -> deleteChild documentBody element

