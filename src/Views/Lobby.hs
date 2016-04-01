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
import Views.Chat


-- |Creates DOM for chaning nick name
-- |Includes an input field and a button.
createChangeNickNameDOM :: LobbyAPI -> Client ()
createChangeNickNameDOM api = do
  nickDiv <- createDiv [("id","nickNameDiv")]

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

  onEvent nickNameField KeyPress $ \13 -> nickUpdateFunction

  clickEventString "nickNameBtn" nickUpdateFunction
  return ()

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

-- |Creates the initial DOM upon entering the lobby
createLobbyDOM :: LobbyAPI -> GameAPI -> Client ()
createLobbyDOM api gapi = do

  lobbyDiv <- createDiv [("id","lobby")]

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


  appendChild createGamebtn crGamebtnText

  playerList <- newElem "div" `with`
    [
      prop "id" =: "playerList"
    ]

  gamesListDiv <- newElem "div" `with`
    [
      attr "id" =: "gamesList"
    ]

  addChildrenToLeftColumn [playerList]
  addChildrenToParent' lobbyDiv [header, gamesListDiv, createGamebtn]
  addChildrenToCenterColumn [lobbyDiv]

  createGameBtn api newGameAPI

  gameList <- onServer $ getGamesList api
  mapM_ (addGame api gapi) gameList

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
      createGameDOM lapi gapi

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
    bool <- onServer $ joinGame api <.> gameName <.> ""
    return ()
  return ()

-- |Updates the list of games that a player can join
updateGamesList :: LobbyAPI -> GameAPI -> Client ()
updateGamesList api gapi = do
  gamesListDiv <- elemById "gamesList"
  case gamesListDiv of
    Just listDiv -> do
      clearChildren listDiv
      gameList <- onServer $ getGamesList api
      mapM_ (addGame api gapi) gameList
    _ -> return ()
