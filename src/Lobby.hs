module Lobby
  where

import Haste
import Haste.DOM
import Haste.Events

import Haste.App

import Data.Maybe
import Data.List

import LobbyTypes

initDOM :: IO ()
initDOM = do
  cssLink <- newElem "link" `with`
    [
      prop "rel"          =: "stylesheet",
      prop "type"         =: "text/css",
      prop "href"         =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
    --prop "integrity"    =: "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7",
      prop "crossorigin"  =: "anonymous"
    ]

  appendChild documentBody cssLink

createBootstrapTemplate :: String -> IO Elem
createBootstrapTemplate parentName = do

  containerDiv <- newElem "div" `with`
    [
      attr "class" =: "container-fluid",
      attr "id"    =: "container-fluid"
    ]

  rowDiv <- newElem "div" `with`
    [
      attr "class" =: "row",
      attr "id"    =: "row"
    ]

  leftPaddingColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-3",
      attr "id"    =: "leftContent"
    ]
  rightPaddingColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-3",
      attr "id"    =: "rightContent"
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

 -- appendChild documentBody cssLink
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

  addChildrenToLeftColumn [playerList]
  addChildrenToCenterColumn [header, createGamebtn]

createGameDOM :: (String,[String]) -> IO ()
createGameDOM (gameId,ps) = do

  parentDiv <- createBootstrapTemplate "lobbyGame"

  nameOfGame <- newTextElem gameId
  header <- newElem "h1" `with`
    [
      style "text-align" =: "center",
      style "margin-left" =: "auto",
      style "margin-right" =: "auto"
    ]
  appendChild header nameOfGame

  --appendChild parentDiv header
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

  addChildrenToLeftColumn [list]
  addChildrenToCenterColumn [header]

createGameDOMWithGame :: LobbyGame -> IO ()
createGameDOMWithGame lobbyGame = createGameDOM (fst lobbyGame, map snd $ snd lobbyGame)

deleteLobbyDOM :: IO ()
deleteLobbyDOM = deleteDOM "container-fluid"

deleteGameDOM :: IO ()
deleteGameDOM = deleteDOM "container-fluid"

deleteDOM :: String -> IO ()
deleteDOM s = withElem s $ \element -> deleteChild documentBody element

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

addChildrenToCenterColumn :: [Elem] -> IO ()
addChildrenToCenterColumn = addChildrenToParent  "centerContent"

addChildrenToLeftColumn :: [Elem] -> IO ()
addChildrenToLeftColumn = addChildrenToParent "leftContent"

addChildrenToRightColumn :: [Elem] -> IO ()
addChildrenToRightColumn = addChildrenToParent "rightContent"

addChildrenToParent :: String -> [Elem] -> IO ()
addChildrenToParent parent children = do
  parentElem <- elemById parent
  mapM_ (appendChild $ fromJust parentElem) children