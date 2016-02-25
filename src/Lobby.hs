-- |Contains all functions related to DOM manipulation
module Lobby
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

-- |Creates the initial DOM upon entering the lobby
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

  playerList <- newElem "div" `with`
    [
      prop "id" =: "playerList"
    ]

  appendChild createGamebtn crGamebtnText
  appendChild parentDiv createGamebtn
  appendChild parentDiv playerList
  appendChild documentBody parentDiv

-- |Creates the DOM for a 'LobbyGame' inside the lobby
-- Useful since the Client is unaware of the specific 'LobbyGame' but can get the name and list with 'Name's of players from the server.
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
    appendChild parentDiv listhead
    mapM_ (\p -> do
                name <- newTextElem $ p ++ " "
                appendChild list name
            ) ps

    createStartGameBtn <- newElem "button" `with`
      [
        prop "id" =: "startGameButton"
      ]
    createStartGameBtnText <- newTextElem "Start game"
    appendChild createStartGameBtn createStartGameBtnText
    appendChild parentDiv createStartGameBtn
    appendChild parentDiv list
    appendChild documentBody parentDiv

-- |Creates the DOM for a 'LobbyGame' inside the lobby given that 'LobbyGame'
createGameDOMWithGame :: LobbyGame -> IO ()
createGameDOMWithGame lobbyGame = do
  game <- CC.readMVar lobbyGame
  createGameDOM (fst game, map snd $ snd game)

-- |Deletes the DOM created for the intial lobby view
deleteLobbyDOM :: IO ()
deleteLobbyDOM = deleteDOM "lobby"

-- |Deletes the DOM created for a game in the lobby
deleteGameDOM :: IO ()
deleteGameDOM = deleteDOM "lobbyGame"

-- |Helper function that deletes DOM given an identifier from documentBody
deleteDOM :: String -> IO ()
deleteDOM s = withElems [s] $ \[element] -> deleteChild documentBody element

-- |Creates a button for creating a 'LobbyGame'
createGameBtn :: LobbyAPI -> GameAPI-> Client ()
createGameBtn lapi gapi =
  clickEvent "createGamebtn" onCreateBtnMouseClick
    where
      onCreateBtnMouseClick = do
        maybeStrings <- onServer (createGame lapi)
        case maybeStrings of
          Nothing          -> return ()
          Just gameStrings -> do
            switchToGameDOM gameStrings
            withElem "playerList" $ \pdiv ->
                fork $ listenForChanges (players gameStrings) addPlayerToPlayerlist 1000 pdiv
            clickEvent "startGameButton"
              (do
                gameDiv <- newElem "div" `with`
                  [
                    prop "id" =: "gameDiv"
                  ]
                names <- onServer (players gameStrings)
                startGame gapi names gameDiv
              )


      switchToGameDOM (guid, player) = do
        liftIO deleteLobbyDOM
        liftIO $ createGameDOM (guid, [player])

      players gameStrings = findPlayersInGame lapi <.> fst gameStrings

clickEvent :: String -> Client () -> Client ()
clickEvent identifier fun =
  withElem identifier $ \e -> do
    onEvent e Click $ \(MouseData _ mb _) ->
      case mb of
        Just MouseLeft -> fun
        Nothing        -> return ()
    return ()

-- |Adds DOM for a game
addGame :: LobbyAPI -> String -> Client ()
addGame api gameName =
  withElem "lobby" $ \lobbyDiv -> do
    gameDiv <- newElem "div"
    gameEntry <- newElem "button" `with`
      [
        prop "id" =: gameName
      ]
    textElem <- newTextElem gameName
    appendChild gameEntry textElem
    appendChild gameDiv gameEntry
    appendChild lobbyDiv gameDiv

    clickEvent gameName
      (do
        onServer $ joinGame api <.> gameName
        players <- onServer $ findPlayersInGame api <.> gameName
        liftIO deleteLobbyDOM
        liftIO $ createGameDOM (gameName, players)
        withElem "playerList" $ \pdiv ->
            fork $ listenForChanges (findPlayersInGame api <.> gameName) addPlayerToPlayerlist 1000 pdiv)

    return ()

-- |Queries the server for a list in an interval, applies a function for every item in the list .
listenForChanges :: (Eq a, Binary a) => Remote (Server [a]) -> (Elem -> a -> Client ()) -> Int -> Elem -> Client ()
listenForChanges remoteCall addChildrenToParent updateDelay parent = listenForChanges' []
  where
    listenForChanges' currentData = do
      remoteData <- onServer remoteCall
      if currentData == remoteData
        then
          setTimer (Once updateDelay) $ listenForChanges' currentData
        else
          (do
            clearChildren parent
            mapM_ (addChildrenToParent parent) remoteData
            setTimer (Once updateDelay) $ listenForChanges' remoteData)
      return ()


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
      prop "id" =: gameName
    ]
  textElem <- newTextElem gameName
  appendChild gameEntry textElem
  appendChild gameDiv gameEntry
  appendChild documentBody gameDiv

  clickEvent gameName $ onServer $ joinGame api <.> gameName
  return ()
