-- |Contains all functions related to DOM manipulation
module Views.Lobby
  where

import Haste (Interval(Once), setTimer)
import Haste.App
import Haste.DOM
import Haste.Events

import Data.Maybe
import Data.List

import LobbyTypes
import LobbyAPI
import GameAPI
import Haste.App.Concurrent
import qualified Control.Concurrent as CC

import Views.Game
import Views.Common


-- |Creates the initial DOM upon entering the lobby
createLobbyDOM :: LobbyAPI -> Client ()
createLobbyDOM api = do

  lobbyDiv <- createBootstrapTemplate "lobby"

  createGamebtn <- newElem "button" `with`
    [
      prop "id" =: "createGamebtn"
    ]
  crGamebtnText <- newTextElem "Create new game"

  header <- newElem "h1" `with`
    [
      attr "class" =: "text-center"
    ]

  headerText <- newTextElem "Hastings Lobby"
  appendChild header headerText

  nickDiv <- newElem "div" `with`
    [
      prop "id" =: "nickNameDiv"
    ]
  nickNameText <- newTextElem "Change nick name"
  nickNameField <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id" =: "nickNameField"
    ]
  nickNameButton <- newElem "button" `with`
    [
      attr "id" =: "nickNameBtn"
    ]
  nickNameBtnText <- newTextElem "Change"

  appendChild nickNameButton nickNameBtnText
  appendChild nickDiv nickNameText
  appendChild nickDiv nickNameField
  appendChild nickDiv nickNameButton
  addChildrenToRightColumn [nickDiv]

  appendChild createGamebtn crGamebtnText

  playerList <- newElem "div" `with`
    [
      prop "id" =: "playerList"
    ]

  gamesListDiv <- newElem "div" `with`
    [
      attr "id" =: "gamesList"
    ]

  leftContent <- elemById "leftContent"
  liftIO $ createChatDOM $ fromJust leftContent

  addChildrenToLeftColumn [playerList]
  addChildrenToCenterColumn [header, gamesListDiv, createGamebtn]

  onEvent nickNameField KeyPress $ \13 -> nickUpdateFunction


  clickEventString "nickNameBtn" nickUpdateFunction

  createGameBtn api newGameAPI

  gameList <- onServer $ getGamesList api
  mapM_ (addGame api) gameList

  where
    nickUpdateFunction =
      withElem "nickNameField" $ \field -> do
        newName <- getValue field
        case newName of
          Just ""   -> return ()
          Just name -> do
            setProp field "value" ""
            onServer $ changeNickName api <.> name
          Nothing   -> return ()

createChatDOM :: Elem -> IO ()
createChatDOM parentDiv = do

  br <- newElem "br"

  chatDiv <- newElem "div" `with`
    [
      attr "id" =: "chatDiv"
    ]

  chatBox <- newElem "textarea" `with`
    [
      attr "id"       =: "chatBox",
      attr "rows"     =: "10",
      attr "cols"     =: "18",
      attr "readonly" =: ""
    ]

  messageBox <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id"   =: "messageBox",
      attr "cols" =: "60"
    ]

  appendChild parentDiv chatDiv
  appendChild chatDiv chatBox
  appendChild chatDiv br
  appendChild chatDiv messageBox

-- |Creates a button for creating a 'LobbyGame'
createGameBtn :: LobbyAPI -> GameAPI-> Client ()
createGameBtn lapi gapi = do
  clickEventString "createGamebtn" onCreateBtnMouseClick
  return ()
  where
    onCreateBtnMouseClick = do
      maybeUuid <- onServer $ createGame lapi <.> getMaxNumberOfPlayers gapi
      case maybeUuid of
        Nothing          -> return ()
        Just gameUuid -> do
          switchToGameDOM gameUuid
          clickEventString "startGameButton" $ do
              gameDiv <- newElem "div" `with`
                [
                  prop "id" =: "gameDiv"
                ]
              names <- onServer (findPlayersInGame lapi)
              startGame gapi names gameDiv
          return ()

    switchToGameDOM guid = do
      deleteLobbyDOM
      createGameDOM lapi

-- |Updates the list of players on the client
updatePlayerList :: LobbyAPI -> Client ()
updatePlayerList api = do
  players <- onServer $ getPlayerNameList api
  playerDiv <- elemById "playerList"

  case playerDiv of
    Just parent -> do
      clearChildren parent
      mapM_ (addPlayerToPlayerlist parent) players
    Nothing     -> return ()


-- |Adds the playername followed by a <br> tag to the given parent.
addPlayerToPlayerlist :: Elem -> String -> Client ()
addPlayerToPlayerlist parent name = do
  textElem <- newTextElem name
  br <- newElem "br"
  appendChild parent textElem
  appendChild parent br

-- |Adds the DOM for a list of games
addGameToDOM :: LobbyAPI -> String -> Client ()
addGameToDOM api gameName = do
  gameDiv <- newElem "div"
  gameEntry <- newElem "button" `with`
    [
      prop "id" =: "gameName"
    ]
  textElem <- newTextElem gameName
  appendChild gameEntry textElem
  appendChild gameDiv gameEntry
  appendChild documentBody gameDiv

  clickEventString gameName $ do
    bool <- onServer $ joinGame api <.> gameName
    return ()
  return ()

-- |Updates the list of games that a player can join
updateGamesList :: LobbyAPI -> Client ()
updateGamesList api = do
  gamesListDiv <- elemById "gamesList"
  case gamesListDiv of
    Just listDiv -> do
      clearChildren listDiv
      gameList <- onServer $ getGamesList api
      mapM_ (addGame api) gameList
    _ -> return ()
