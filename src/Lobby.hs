-- |Contains all functions related to DOM manipulation
module Lobby
  where

import Haste
import Haste.App
import Haste.DOM
import Haste.Events

import Data.Maybe
import Data.List

import LobbyTypes
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
createGameBtn :: Remote (Server (String,String)) -> Remote (String -> Server [String]) -> Client ()
createGameBtn createGame fpInG = do
  withElems ["createGamebtn"] $ \[createGamebtn] ->
    onEvent createGamebtn Click $ \(MouseData _ mb _) ->
      case mb of
        Just MouseLeft -> do
          gameStrs <- onServer createGame
          case fst gameStrs of
            "false" -> return ()
            _       -> do
              liftIO deleteLobbyDOM
              liftIO $ createGameDOM (fst gameStrs, [snd gameStrs])
              withElem "playerList" $ \pdiv ->
                  fork $ listenForChanges (fpInG <.> fst gameStrs) addPlayerToPlayerlist 1000 pdiv
        _ -> return ()
  return ()

-- |Adds DOM for a game
addGame :: Remote (String -> Server ()) -> String -> Client ()
addGame joinGame gameName =
  withElems ["lobby"] $ \[lobbyDiv] -> do
    gameDiv <- newElem "div"
    gameEntry <- newElem "button" `with`
      [
        prop "id" =: gameName
      ]
    textElem <- newTextElem gameName
    appendChild gameEntry textElem
    appendChild gameDiv gameEntry
    appendChild lobbyDiv gameDiv

    _ <- ($)
      withElems [gameName] $ \[gameButton] ->
        onEvent gameButton Click (\(MouseData _ mb _) ->
          case mb of
            Just MouseLeft ->
              onServer $ joinGame <.> gameName
            _ -> return ())

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
addGameToDOM :: Remote (String -> Server ()) -> String -> Client ()
addGameToDOM joinGame gameName = do
  gameDiv <- newElem "div"
  gameEntry <- newElem "button" `with`
    [
      prop "id" =: gameName
    ]
  textElem <- newTextElem gameName
  appendChild gameEntry textElem
  appendChild gameDiv gameEntry
  appendChild documentBody gameDiv

  _ <- ($)
    withElems [gameName] $ \[gameButton] ->
      onEvent gameButton Click (\(MouseData _ mb _) ->
        case mb of
          Just MouseLeft ->
            onServer $ joinGame <.> gameName
          _ -> return ())

  return ()
