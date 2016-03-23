module Views.Game
  where

import Haste.App
import Haste.DOM
import Haste.Events
import Haste.App.Concurrent

import LobbyTypes
import LobbyAPI
import Views.Common

-- |Creates the DOM for a 'LobbyGame' inside the lobby
-- Useful since the Client is unaware of the specific 'LobbyGame' but can get the name and list with 'Name's of players from the server.
createGameDOM :: LobbyAPI -> Client ()
createGameDOM api = do
  parentDiv <- createBootstrapTemplate "lobbyGame"
  gameName <- onServer $ findGameName api
  players <- onServer $ findPlayersInGame api
  nameOfGame <- newTextElem gameName
  header <- newElem "h1" `with`
    [
      attr "id" =: "gameHeader",
      style "text-align" =: "center",
      style "margin-left" =: "auto",
      style "margin-right" =: "auto"
    ]
  appendChild header nameOfGame

  createStartGameBtn <- newElem "button" `with`
    [
      prop "id" =: "startGameButton"
    ]
  createStartGameBtnText <- newTextElem "Start game"
  appendChild createStartGameBtn createStartGameBtnText

  list <- newElem "div" `with`
    [
      prop "id" =: "playerList"
    ]
  listhead <- newTextElem "Players: "
  appendChild list listhead

  mapM_ (\p -> do
              name <- newTextElem $ p ++ " "
              appendChild list name
        ) players

  mapM_ (addPlayerWithKickToPlayerlist api list) players

  gameNameDiv <- newElem "div"
  gameNameText <- newTextElem "Change game name"
  gameNameField <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id" =: "gameNameField"
    ]
  gameNameButton <- newElem "button" `with`
    [
      attr "id" =: "gameNameBtn"
    ]
  gameNameBtnText <- newTextElem "Change"
  appendChild gameNameButton gameNameBtnText

  appendChild gameNameDiv gameNameText
  appendChild gameNameDiv gameNameField
  appendChild gameNameDiv gameNameButton

  addChildrenToLeftColumn [createStartGameBtn, list]
  addChildrenToRightColumn [gameNameDiv]
  addChildrenToCenterColumn [header]

  onEvent gameNameField KeyPress $ \13 -> gameUpdateFunction

  clickEventString "gameNameBtn" gameUpdateFunction

  return ()

  where
    gameUpdateFunction =
      withElem "gameNameField" $ \field -> do
        newName <- getValue field
        case newName of
          Just ""   -> return ()
          Just name -> do
            setProp field "value" ""
            onServer $ changeGameName api <.> name
          Nothing   -> return ()

-- |Convenience function for calling on the kick function.
kickFunction :: Name -> LobbyAPI -> Client ()
kickFunction name api = onServer $ kickPlayer api <.> name

-- |Adds the playername and a button to kick them followed by a <br> tag to the given parent.
addPlayerWithKickToPlayerlist :: LobbyAPI -> Elem -> String -> Client ()
addPlayerWithKickToPlayerlist api parent name = do
  textElem <- newTextElem name
  br <- newElem "br"
  kickBtn <- newElem "button"
  kick <- newTextElem "kick"
  clickEventElem kickBtn $ kickFunction name api
  appendChild kickBtn kick
  appendChild parent textElem
  appendChild parent kickBtn
  appendChild parent br

-- |Adds DOM for a game
addGame :: LobbyAPI -> String -> Client ()
addGame api gameID =
  withElems ["lobby", "centerContent", "createGamebtn"] $ \[lobbyDiv, centerContent, createGamebtn] -> do
    gameDiv <- newElem "div"
    gameName <- onServer $ findGameName api
    gameEntry <- newElem "button" `with`
      [
        prop "id" =: gameName
      ]
    textElem <- newTextElem gameName
    appendChild gameEntry textElem
    appendChild gameDiv gameEntry
    insertChildBefore centerContent createGamebtn gameDiv

    clickEventString gameName $ do
      onServer $ joinGame api <.> gameID
      players <- onServer $ findPlayersInGame api
      liftIO deleteLobbyDOM
      createGameDOM api
      withElem "playerList" $ \pdiv ->
          fork $ listenForChanges (findPlayersInGame api) (addPlayerWithKickToPlayerlist api) 1000 pdiv

    return ()
