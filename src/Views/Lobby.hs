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
import Haste.App.Concurrent
import qualified Control.Concurrent as CC
import Control.Monad (when)

import Views.Game
import Views.Common
import Views.Chat


-- |Creates DOM for chaning nick name
-- |Includes an input field and a button.
createChangeNickNameDOM :: LobbyAPI -> Client ()
createChangeNickNameDOM api = createInputFieldWithButton "nick-name" "Nick name" nickUpdateFunction
  where
    nickUpdateFunction =
      withElem "nick-name-field" $ \field -> do
        newName <- getValue field
        case newName of
          Just ""   -> return ()
          Just name -> do
            setProp field "value" ""
            onServer $ changeNickName api <.> name
          Nothing   -> return ()

-- |Creates the initial DOM upon entering the lobby
createLobbyDOM :: LobbyAPI -> Client ()
createLobbyDOM api = do

  lobbyDiv <- createDiv [("id","lobby")]

  createGamebtn <- newElem "button" `with`
    [
      attr "id"    =: "create-game-btn",
      attr "class" =: "btn btn-default"
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
      prop "id" =: "player-list"
    ]

  (gamesListDiv, _) <- createTable "games-list" 500 ["Name"]

  addChildrenToLeftColumn [playerList]
  addChildrenToParent' lobbyDiv [header, gamesListDiv, createGamebtn]
  addChildrenToCenterColumn [lobbyDiv]

  createGameBtn api

  gameList <- onServer $ getGamesList api
  mapM_ (addGameDOM api) gameList

-- |Creates a button for creating a 'LobbyGame'
createGameBtn :: LobbyAPI -> Client ()
createGameBtn lapi = do
  clickEventString "create-game-btn" onCreateBtnMouseClick
  return ()
  where
    onCreateBtnMouseClick = do
      maybeUuid <- onServer $ createGame lapi <.> 6
      case maybeUuid of
        Nothing          -> return ()
        Just gameUuid -> do
          switchToGameDOM gameUuid
          return ()

    switchToGameDOM guid = do
      deleteLobbyDOM
      createGameDOM lapi

-- |Updates the list of players on the client
updatePlayerList :: LobbyAPI -> Client ()
updatePlayerList api = do
  players <- onServer $ getPlayerNameList api
  playerDiv <- elemById "player-list"

  case playerDiv of
    Just parent -> do
      clearChildren parent
      mapM_ (addPlayerToPlayerlist parent) players
    Nothing     -> return ()


-- |Adds DOM for a game
addGameDOM :: LobbyAPI -> String -> Client ()
addGameDOM api gameID = do
  maybeGameListDiv <- elemById "games-list-table-body"
  case maybeGameListDiv of
    Nothing -> return ()
    Just gameListDiv -> do
      gameName <- onServer $ findGameNameWithID api <.> gameID
      tr <- newElem "tr"
      tdBtn <- newElem "td"
      gameEntry <- newElem "button" `with`
        [
          attr "class" =: "btn btn-default"
        ]
      textElemBtn <- newTextElem "Join"

      textElemName <- newTextElem gameName
      tdName <- newElem "td"

      appendChild gameEntry textElemBtn
      appendChild tdBtn gameEntry
      appendChild tdName textElemName
      addChildrenToParent' tr [tdName, tdBtn]
      appendChild gameListDiv tr

      clickEventElem gameEntry $ do
        hasPassword <- onServer $ isGamePasswordProtected api <.> gameID
        if hasPassword then do
          password <- prompt "Enter password"
          joinGameClient password
        else
          joinGameClient ""
      return ()
  where
    joinGameClient password = do
      allowedToJoin <- onServer $ joinGame api <.> gameID <.> password
      when allowedToJoin $ do
          deleteLobbyDOM
          createGameDOM api

-- |Adds the playername followed by a <br> tag to the given parent.
addPlayerToPlayerlist :: Elem -> String -> Client ()
addPlayerToPlayerlist parent name = do
  textElem <- newTextElem name
  br <- newElem "br"
  appendChild parent textElem
  appendChild parent br

-- |Updates the list of games that a player can join
updateGamesList :: LobbyAPI -> Client ()
updateGamesList api = do
  gamesListDiv <- elemById "games-list-table-body"
  case gamesListDiv of
    Just listDiv -> do
      clearChildren listDiv
      gameList <- onServer $ getGamesList api
      mapM_ (addGameDOM api) gameList
    _ -> return ()
