module Lobby
  where

import Haste
import Haste.DOM
import Haste.Events

import Haste.App

import Data.Maybe
import Data.List

import LobbyTypes

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

createGameBtn :: Remote (Server (String,String)) -> Client ()
createGameBtn createGame = do
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
        _ -> return ()
  return ()

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

--Queries the server for a list in an interval, applies a function for every item in the list .
listenForChanges :: Remote (Server [String]) -> (Elem -> String -> Client ()) -> Int -> Elem -> Client ()
listenForChanges remoteCall addChildrenToParent updateDelay parent = listenForChanges' []
  where
    listenForChanges' :: [String] -> Client ()
    listenForChanges' currentData = do
      remoteData <- onServer remoteCall
      case currentData == remoteData of
        False -> do
          clearChildren parent
          mapM_ (addChildrenToParent parent) remoteData
          setTimer (Once updateDelay) $ listenForChanges' remoteData

        True -> setTimer (Once updateDelay) $ listenForChanges' currentData

      return ()


-- Adds the playername followed by a <br> tag to the given parent.
addPlayerToPlayerlist :: Elem -> String -> Client ()
addPlayerToPlayerlist parent name = do
  textElem <- newTextElem name
  br <- newElem "br"
  appendChild parent textElem
  appendChild parent br
