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

initDOM :: Client ()
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

createBootstrapTemplate :: String -> Client Elem
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

  appendChild documentBody containerDiv
  appendChild containerDiv rowDiv
  appendChild rowDiv parentDiv
  appendChild parentDiv leftPaddingColDiv
  appendChild parentDiv centerColDiv
  appendChild parentDiv rightPaddingColDiv

  return parentDiv
-- |Creates the initial DOM upon entering the lobby
createLobbyDOM :: LobbyAPI -> Client ()
createLobbyDOM api = do

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

  nickDiv <- newElem "div" `with`
    [
      prop "id" =: "nickNameDiv"
    ]
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

  appendChild createGamebtn crGamebtnText

  playerList <- newElem "div" `with`
    [
      prop "id" =: "playerList"
    ]

-- <<<<<<< HEAD
  --appendChild createGamebtn crGamebtnText
  --appendChild parentDiv createGamebtn
  --appendChild parentDiv playerList

  --createChatDOM parentDiv

  --appendChild documentBody parentDiv
-- =======
  addChildrenToLeftColumn [playerList]
  addChildrenToCenterColumn [header, createGamebtn]

  clickEventString "nickNameBtn" $
    withElem "nickNameField" $ \field -> do
      newName <- getValue field
      case newName of
        Just name -> onServer $ changeNickName api <.> name
        Nothing -> return ()

-- |Creates the DOM for a 'LobbyGame' inside the lobby given that 'LobbyGame'
createGameDOMWithGame :: LobbyAPI -> LobbyGame -> Client ()
createGameDOMWithGame  api lobbyGame = do
  game <- liftIO $ CC.readMVar lobbyGame
  createGameDOM api (fst game, map name $ snd game)
-- >>>>>>> development

createChatDOM :: Elem -> IO ()
createChatDOM parentDiv = do

  br <- newElem "br"

  chatDiv <- newElem "div" `with`
    [
      attr "id" =: "chatDiv"
    ]

  chatBox <- newElem "textarea" `with`
    [
      attr "id"       =: "chatBox",
      attr "rows"     =: "10",
      attr "cols"     =: "60",
      attr "readonly" =: ""
    ]

  messageBox <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id"   =: "messageBox",
      attr "cols" =: "60"
    ]

  appendChild parentDiv chatDiv
  appendChild chatDiv chatBox
  appendChild chatDiv br
  appendChild chatDiv messageBox


-- |Creates the DOM for a 'LobbyGame' inside the lobby given that 'LobbyGame'
--createGameDOMWithGame :: LobbyGame -> IO ()
--createGameDOMWithGame lobbyGame = do
  --game <- CC.readMVar lobbyGame
  --createGameDOM (fst game, map name $ snd game)

-- |Creates the DOM for a 'LobbyGame' inside the lobby
-- Useful since the Client is unaware of the specific 'LobbyGame' but can get the name and list with 'Name's of players from the server.
createGameDOM :: LobbyAPI -> (String,[String]) -> Client ()
createGameDOM api (gameID,ps) = do
  parentDiv <- createBootstrapTemplate "lobbyGame"

  nameOfGame <- newTextElem gameID
  header <- newElem "h1" `with`
    [
      style "text-align" =: "center",
      style "margin-left" =: "auto",
      style "margin-right" =: "auto"
    ]
  appendChild header nameOfGame

  createStartGameBtn <- newElem "button" `with`
    [
      prop "id" =: "startGameButton"
    ]
  createStartGameBtnText <- newTextElem "Start game"

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

  mapM_ (addPlayerWithKickToPlayerlist api gameID list) ps

  addChildrenToLeftColumn [createStartGameBtn, list]
  addChildrenToCenterColumn [header]

-- |Deletes the DOM created for the intial lobby view
deleteLobbyDOM :: IO ()
deleteLobbyDOM = deleteDOM "container-fluid"

-- |Deletes the DOM created for a game in the lobby
deleteGameDOM :: IO ()
deleteGameDOM = deleteDOM "container-fluid"

-- |Helper function that deletes DOM given an identifier from documentBody
deleteDOM :: String -> IO ()
deleteDOM s = withElem s $ \element -> deleteChild documentBody element

-- |Creates a button for creating a 'LobbyGame'
createGameBtn :: LobbyAPI -> GameAPI-> Client ()
createGameBtn lapi gapi =
  clickEventString "createGamebtn" onCreateBtnMouseClick
    where
      onCreateBtnMouseClick = do
        maybeStrings <- onServer (createGame lapi)
        case maybeStrings of
          Nothing          -> return ()
          Just gameStrings -> do
            switchToGameDOM gameStrings
            withElem "playerList" $ \pdiv ->
                fork $ listenForChanges (players gameStrings) (changeWithKicks gameStrings) 1000 pdiv
            clickEventString "startGameButton" $ do
                gameDiv <- newElem "div" `with`
                  [
                    prop "id" =: "gameDiv"
                  ]
                names <- onServer (players gameStrings)
                startGame gapi names gameDiv

      switchToGameDOM (guid, player) = do
        liftIO deleteLobbyDOM
        createGameDOM lapi (guid, [player])

      players gameStrings = findPlayersInGame lapi <.> fst gameStrings

      changeWithKicks (guid, _) = addPlayerWithKickToPlayerlist lapi guid

-- |Creates a listener for a click event with the Elem with the given String and a function.
clickEventString :: String -> Client () -> Client ()
clickEventString identifier fun =
  withElem identifier $ \e -> do
    clickEventElem e fun
    return ()

-- |Creates a listener for a click event with the given 'Elem' and a function.
clickEventElem :: Elem -> Client () -> Client HandlerInfo
clickEventElem e fun =
   onEvent e Click $ \(MouseData _ mb _) ->
      case mb of
        Just MouseLeft -> fun
        Nothing        -> return ()

-- |Adds DOM for a game
addGame :: LobbyAPI -> String -> Client ()
addGame api gameName =
  withElems ["lobby", "centerContent", "createGamebtn"] $ \[lobbyDiv, centerContent, createGamebtn] -> do
    gameDiv <- newElem "div"
    gameEntry <- newElem "button" `with`
      [
        prop "id" =: gameName
      ]
    textElem <- newTextElem gameName
    appendChild gameEntry textElem
    appendChild gameDiv gameEntry
    insertChildBefore centerContent createGamebtn gameDiv

    clickEventString gameName $ do
        onServer $ joinGame api <.> gameName
        players <- onServer $ findPlayersInGame api <.> gameName
        liftIO deleteLobbyDOM
        createGameDOM api (gameName, players)
        withElem "playerList" $ \pdiv ->
            fork $ listenForChanges (findPlayersInGame api <.> gameName) (addPlayerWithKickToPlayerlist api gameName) 1000 pdiv

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

-- |Convenience function for calling on the kick function.
kickFunction :: String -> Name -> LobbyAPI -> Client ()
kickFunction string name api = onServer $ kickPlayer api <.> string <.> name

-- |Adds the playername and a button to kick them followed by a <br> tag to the given parent.
addPlayerWithKickToPlayerlist :: LobbyAPI -> String -> Elem -> String -> Client ()
addPlayerWithKickToPlayerlist api gameID parent name = do
  textElem <- newTextElem name
  br <- newElem "br"
  kickBtn <- newElem "button"
  kick <- newTextElem "kick"
  clickEventElem kickBtn $ kickFunction gameID name api
  appendChild kickBtn kick
  appendChild parent textElem
  appendChild parent kickBtn
  appendChild parent br

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

  clickEventString gameName $ onServer $ joinGame api <.> gameName
  return ()

addChildrenToCenterColumn :: [Elem] -> Client ()
addChildrenToCenterColumn = addChildrenToParent  "centerContent"

addChildrenToLeftColumn :: [Elem] -> Client ()
addChildrenToLeftColumn = addChildrenToParent "leftContent"

addChildrenToRightColumn :: [Elem] -> Client ()
addChildrenToRightColumn = addChildrenToParent "rightContent"

addChildrenToParent :: String -> [Elem] -> Client ()
addChildrenToParent parent children = do
  parentElem <- elemById parent
  mapM_ (appendChild $ fromJust parentElem) children
