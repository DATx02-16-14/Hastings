module Lobby
  where
import Haste.App
import LobbyServer
import Haste.DOM
import Haste.Events
import Data.Maybe
import LobbyTypes

createLobbyDOM :: IO ()
createLobbyDOM = do
  lobbyDiv <- newElem "div" `with`
    [
      prop "id" =: "lobby"
    ]
  createGamebtn <- newElem "button" `with`
    [
      prop "id" =: "createGamebtn"
    ]
  crGamebtnText <- newTextElem "Create new game"

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
      attr "class" =: "row"
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
      attr "class" =: "col-md-6"
    ]

  header <- newElem "h1" `with`
    [
      attr "class" =: "text-center"
    ]

  headerText <- newTextElem "Hastings Lobby"
  appendChild header headerText

  appendChild documentBody cssLink
  appendChild documentBody containerDiv
  appendChild containerDiv rowDiv
  appendChild rowDiv lobbyDiv
  appendChild lobbyDiv header
  appendChild lobbyDiv leftPaddingColDiv
  appendChild lobbyDiv centerColDiv
  appendChild lobbyDiv rightPaddingColDiv


  appendChild createGamebtn crGamebtnText
  appendChild centerColDiv createGamebtn

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
