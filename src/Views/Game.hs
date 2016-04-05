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
import GameAPI

import Text.Read

-- |Creates the DOM for a 'LobbyGame' inside the lobby
-- Useful since the Client is unaware of the specific 'LobbyGame' but can get the name and list with 'Name's of players from the server.
createGameDOM :: LobbyAPI -> GameAPI -> Client ()
createGameDOM api gapi = do
  parentDiv <- createDiv [("id","lobbyGame")]

  createGameChangeNameDOM api
  createUpdateMaxNumberPlayersDOM api gapi

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
      attr "id"    =: "startGameButton",
      attr "class" =: "btn btn-default"
    ]
  createStartGameBtnText <- newTextElem "Start game"
  appendChild createStartGameBtn createStartGameBtnText

  list <- newElem "div" `with`
    [
      attr "id" =: "gamePlayerList"
    ]
  listhead <- newTextElem "Players: "
  br <- newElem "br"
  appendChild list listhead
  appendChild list br

  addPlayersToPlayerList api list players

  addChildrenToParent' parentDiv [header, list, createStartGameBtn]
  addChildrenToCenterColumn [parentDiv]

-- |Creates the DOM for chaning the name of a game.
-- |It includes an input field and a button.
createGameChangeNameDOM :: LobbyAPI -> Client ()
createGameChangeNameDOM api = do
  gameNameDiv <- createDiv [("id", "changeGameName")]
  gameNameText <- newTextElem "Change game name"
  gameNameField <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id"   =: "gameNameField"
    ]
  gameNameButton <- newElem "button" `with`
    [
      attr "id" =: "gameNameBtn"
    ]
  gameNameBtnText <- newTextElem "Change"
  appendChild gameNameButton gameNameBtnText

  addChildrenToParent' gameNameDiv [gameNameText, gameNameField, gameNameButton]

  addChildrenToRightColumn [gameNameDiv]

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

-- |Creates DOM for the updatin the maximum number of players in a game
-- |Contains an input field and a button. Is placed in the right sidebar
createUpdateMaxNumberPlayersDOM :: LobbyAPI -> GameAPI-> Client ()
createUpdateMaxNumberPlayersDOM api gapi = do
  maxNumberDiv <- createDiv [("id","maxNumberDiv")]

  maxNumberText <- newTextElem "Change maximum number of players"
  maxNumberField <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id" =: "maxNumberField"
    ]
  maxNumberButton <- newElem "button" `with`
    [
      attr "id" =: "maxNumberBtn"
    ]
  maxNumberBtnText <- newTextElem "Change"

  appendChild maxNumberButton maxNumberBtnText
  appendChild maxNumberDiv maxNumberText
  appendChild maxNumberDiv maxNumberField
  appendChild maxNumberDiv maxNumberButton
  addChildrenToRightColumn [maxNumberDiv]

  onEvent maxNumberField KeyPress $ \13 -> maxNumberUpdateFunction

  clickEventString "maxNumberBtn" maxNumberUpdateFunction
  return ()

  where
    maxNumberUpdateFunction =
      withElem "maxNumberField" $ \field -> do
        newNumber <- getValue field
        case newNumber of
          Just ""   -> return ()
          Just numberString ->
            case readMaybe numberString of
              Nothing -> return ()
              Just number | number <= getMaxNumberOfPlayers gapi -> do
                setProp field "value" ""
                onServer $ changeMaxNumberOfPlayers api <.> number
                          | otherwise -> return ()
          Nothing   -> return ()

-- |Adds the list of 'Name' to the list of players with
-- Also adds a kick button if the current player is owner of the game
addPlayersToPlayerList :: LobbyAPI -> Elem -> [Name] -> Client ()
addPlayersToPlayerList api parent = addPlayersToPlayerList' 0
  where
    addPlayersToPlayerList' :: Int -> [Name] -> Client ()
    addPlayersToPlayerList' i []     = return ()
    addPlayersToPlayerList' i (name:names) = do
      textElem <- newTextElem name
      br <- newElem "br"
      kickBtn <- newElem "button" `with`
        [
          attr "class" =: "btn btn-default"
        ]
      kick <- newTextElem "kick"
      clickEventElem kickBtn $ onServer $ kickPlayer api <.> i
      appendChild kickBtn kick
      addChildrenToParent' parent [textElem, kickBtn, br]
      addPlayersToPlayerList' (i+1) names

-- |Adds DOM for a game
addGame :: LobbyAPI -> GameAPI -> String -> Client ()
addGame api gapi gameID = do
  maybeGameListDiv <- elemById "gamesListTableBody"
  case maybeGameListDiv of
    Nothing -> return ()
    Just gameListDiv -> do
      gameName <- onServer $ findGameNameWithID api <.> gameID
      tr <- newElem "tr"
      tdBtn <- newElem "td"
      gameEntry <- newElem "button" `with`
        [
          attr "id"    =: gameName,
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

      clickEventString gameName $ do
        didJoinGame <- onServer $ joinGame api <.> gameID
        when didJoinGame $ do
            deleteLobbyDOM
            createGameDOM api gapi
      return ()

-- |Updates the list of players in a game on the client
updatePlayerListGame :: LobbyAPI -> Client ()
updatePlayerListGame api = do
  playerDiv <- elemById "gamePlayerList"
  case playerDiv of
    Just parent -> do
      players <- onServer $ findPlayersInGame api
      clearChildren parent
      br <- newElem "br"
      text <- newTextElem "Players:"
      addChildrenToParent "gamePlayerList" [text, br]
      addPlayersToPlayerList api parent players
    Nothing     -> return ()

-- |Updates the game header with the value at the server
updateGameHeader :: LobbyAPI -> Client ()
updateGameHeader api = do
  maybeHeader <- elemById "gameHeader"
  case maybeHeader of
    Nothing     -> return ()
    Just parent -> do
      gameName <- onServer $ findGameName api
      clearChildren parent
      gameNameText <- newTextElem gameName
      appendChild parent gameNameText
