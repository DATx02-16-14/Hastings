module Views.Game
  where

import Haste.App
import Haste.DOM
import Haste.Events
import Haste.App.Concurrent
import Control.Monad (when)

import LobbyTypes
import LobbyAPI
import Views.Common

import Text.Read

-- |Creates the DOM for a 'LobbyGame' inside the lobby
-- Useful since the Client is unaware of the specific 'LobbyGame' but can get the name and list with 'Name's of players from the server.
createGameDOM :: LobbyAPI -> Client ()
createGameDOM api = do
  parentDiv <- createDiv [("id","lobby-game")]

  isOwner <- onServer $ isOwnerOfCurrentGame api
  when isOwner $ createGameOwnerDOM api

  gameName <- onServer $ findGameName api
  players <- onServer $ findPlayersInGame api
  nameOfGame <- newTextElem gameName
  header <- newElem "h1" `with`
    [
      attr "id" =: "game-header",
      style "text-align" =: "center",
      style "margin-left" =: "auto",
      style "margin-right" =: "auto"
    ]
  appendChild header nameOfGame

  buttonGroup <- newElem "div" `with`
    [
      attr "class" =: "btn-group",
      attr "role"  =: "group"
    ]
  startGameBtn <- newElem "button" `with`
    [
      attr "id"    =: "start-game-button",
      attr "class" =: "btn btn-primary"
    ]
  createStartGameBtnText <- newTextElem "Start game"
  clickEventElem startGameBtn . onServer $ startGame api
  appendChild startGameBtn createStartGameBtnText

  (tableDiv, tableBody) <- createTable "game-player-list" 500 ["Players"]

  leaveGameButton <- newElem "button" `with`
    [
      attr "class" =: "btn btn-danger"
    ]
  leaveGameText <- newTextElem "Leave"
  appendChild leaveGameButton leaveGameText
  clickEventElem leaveGameButton $ onServer $ leaveGame api


  addChildrenToParent' buttonGroup [leaveGameButton, startGameBtn]

  addPlayersToPlayerList api tableBody players

  addChildrenToParent' parentDiv [header, tableDiv, buttonGroup]
  addChildrenToCenterColumn [parentDiv]

-- |Creates DOM for changing game settings
createGameOwnerDOM :: LobbyAPI -> Client ()
createGameOwnerDOM api = do
  createGameChangeNameDOM api
  createUpdateMaxNumberPlayersDOM api
  createSetPasswordDOM api

-- |Creates the DOM for chaning the name of a game.
-- |It includes an input field and a button.
createGameChangeNameDOM :: LobbyAPI -> Client ()
createGameChangeNameDOM api = createInputFieldWithButton "game-name" "Game name" gameUpdateFunction
  where
    gameUpdateFunction =
      withElem "game-name-field" $ \field -> do
        newName <- getValue field
        case newName of
          Just ""   -> return ()
          Just name -> do
            setProp field "value" ""
            onServer $ changeGameName api <.> name
          Nothing   -> return ()

-- |Creates DOM for the updatin the maximum number of players in a game
-- |Contains an input field and a button. Is placed in the right sidebar
createUpdateMaxNumberPlayersDOM :: LobbyAPI -> Client ()
createUpdateMaxNumberPlayersDOM api =
  createInputFieldWithButton "max-number" "Maximum number of players" maxNumberUpdateFunction
  where
    maxNumberUpdateFunction =
      withElem "max-number-field" $ \field -> do
        newNumber <- getValue field
        case newNumber of
          Just ""   -> return ()
          Just numberString ->
            case readMaybe numberString of
              Nothing -> return ()
              Just number | number <= 6 -> do --TODO
                setProp field "value" ""
                onServer $ changeMaxNumberOfPlayers api <.> number
                          | otherwise -> return ()
          Nothing   -> return ()

-- |Creates an input field for setting the password of a game.
-- |Contains an input field and a button. Is placed in right sidebar.
createSetPasswordDOM :: LobbyAPI -> Client ()
createSetPasswordDOM api = createInputFieldWithButton "set-password" "Set password" setPasswordFunction
  where
    setPasswordFunction =
      withElem "set-password-field" $ \field -> do
        maybePassword <- getValue field
        case maybePassword of
          Nothing             -> return ()
          Just ""             -> return ()
          Just passwordString -> do
            setProp field "value" ""
            onServer $ setPassword api <.> passwordString


-- |Adds the list of 'Name' to the list of players with
-- Also adds a kick button if the current player is owner of the game
addPlayersToPlayerList :: LobbyAPI -> Elem -> [Name] -> Client ()
addPlayersToPlayerList api parent names = do
  isOwner <- onServer $ isOwnerOfCurrentGame api
  addPlayersToPlayerList' isOwner names
  where
    addPlayersToPlayerList' :: Bool -> [Name] -> Client ()
    addPlayersToPlayerList' isOwner []           = return ()
    addPlayersToPlayerList' isOwner (name:names) = do
      tr <- newElem "tr"
      tdText <- newElem "td"
      textElem <- newTextElem name
      appendChild tdText textElem
      appendChild tr tdText
      when isOwner $ do
        tdBtn <- newElem "td"
        kickBtn <- newElem "button" `with`
          [
            attr "class" =: "btn btn-warning"
          ]
        kick <- newTextElem "Kick"
        clickEventElem kickBtn $ onServer $ kickPlayer api <.> name
        appendChild kickBtn kick
        appendChild tdBtn kickBtn
        appendChild tr tdBtn

      appendChild parent tr

      addPlayersToPlayerList' isOwner names

-- |Updates the list of players in a game on the client
updatePlayerListGame :: LobbyAPI -> Client ()
updatePlayerListGame api = do
  playerDiv <- elemById "game-player-list-table-body"
  case playerDiv of
    Just parent -> do
      players <- onServer $ findPlayersInGame api
      clearChildren parent
      addPlayersToPlayerList api parent players
    Nothing     -> return ()

-- |Updates the game header with the value at the server
updateGameHeader :: LobbyAPI -> Client ()
updateGameHeader api = do
  maybeHeader <- elemById "game-header"
  case maybeHeader of
    Nothing     -> return ()
    Just parent -> do
      gameName <- onServer $ findGameName api
      clearChildren parent
      gameNameText <- newTextElem gameName
      appendChild parent gameNameText
