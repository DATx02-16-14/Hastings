module Lobby
  where

import Haste
import Haste.DOM
import Haste.Events

import Haste.App

import Data.Maybe
import Data.List

import LobbyTypes

createBootstrapTemplate :: String -> IO Elem
createBootstrapTemplate parentName = do
  cssLink <- newElem "link" `with`
    [
      prop "rel"          =: "stylesheet",
      prop "type"         =: "text/css",
      prop "href"         =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
    --prop "integrity"    =: "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7",
      prop "crossorigin"  =: "anonymous"
    ]

  containerDiv <- newElem "div" `with`
    [
      attr "class" =: "container-fluid"
    ]

  rowDiv <- newElem "div" `with`
    [
      attr "class" =: "row",
      attr "id"    =: "row"
    ]

  leftPaddingColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-3"
    ]
  rightPaddingColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-3"
    ]

  centerColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-6",
      attr "id"    =: "centerContent"
    ]

  parentDiv <- newElem "div" `with`
    [
      prop "id" =: parentName
    ]

  appendChild documentBody cssLink
  appendChild documentBody containerDiv
  appendChild containerDiv rowDiv
  appendChild rowDiv parentDiv
  appendChild parentDiv leftPaddingColDiv
  appendChild parentDiv centerColDiv
  appendChild parentDiv rightPaddingColDiv

  return parentDiv

createLobbyDOM :: IO ()
createLobbyDOM = do

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

  appendChild createGamebtn crGamebtnText

  playerList <- newElem "div" `with`
    [
      prop "id" =: "playerList"
    ]

  withElems ["centerContent"] $ \[contentDiv] -> do
    appendChild contentDiv header
    appendChild contentDiv createGamebtn
    appendChild contentDiv playerList


  appendChild createGamebtn crGamebtnText

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
deleteDOM s = withElems [s, "row"] $ \[element, row] -> deleteChild row element

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
  withElems ["centerContent", "createGamebtn"] $ \[centerContent, createGamebtn] -> do
    gameDiv <- newElem "div"
    gameEntry <- newElem "button" `with`
      [
        prop "id" =: gameName
      ]
    textElem <- newTextElem gameName
    appendChild gameEntry textElem
    appendChild gameDiv gameEntry
    insertChildBefore centerContent createGamebtn gameDiv

    _ <- ($)
      withElems [gameName] $ \[gameButton] ->
        onEvent gameButton Click (\(MouseData _ mb _) ->
          case mb of
            Just MouseLeft ->
              onServer $ joinGame <.> gameName
            _ -> return ())

    return ()

--Queries the server for a list in an interval, applies a function for every item in the list .
listenForChanges :: (Eq a, Binary a) => Remote (Server [a]) -> (Elem -> a -> Client ()) -> Int -> Elem -> Client ()
listenForChanges remoteCall addChildrenToParent updateDelay parent = listenForChanges' []
  where
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
