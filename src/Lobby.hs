module Lobby
  where
import Haste.App
import LobbyServer
import Haste.DOM
import LobbyTypes()
import qualified Control.Concurrent as CC

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
createGameDOMWithGame lobbyGame = do
  game <- CC.readMVar lobbyGame
  createGameDOM (fst game, map snd $ snd game)

deleteLobbyDOM :: IO ()
deleteLobbyDOM = deleteDOM "lobby"

deleteGameDOM :: IO ()
deleteGameDOM = deleteDOM "lobbyGame"

deleteDOM :: String -> IO ()
deleteDOM s = withElems [s] $ \[element] -> deleteChild documentBody element
