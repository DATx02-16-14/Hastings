module Main
    where
import Haste.App
import Haste.App.Standalone
import Lobby
import qualified LobbyServer as Server
import qualified Control.Concurrent as CC
import Haste.Events
import Haste.DOM

main :: IO ()
main = runStandaloneApp $ do
  playersList <- liftServerIO $ CC.newMVar []
  gamesList <- liftServerIO $ CC.newMVar []

  handshake <- remote $ Server.handshake playersList
  createGame <- remote $ Server.createGame gamesList playersList
  getGamesList <- remote $ Server.getGamesList gamesList
  joinGame <- remote $ Server.playerJoinGame playersList gamesList
  onSessionEnd $ Server.closeConnection playersList

  runClient $ do
    liftIO createLobbyDOM

    name <- prompt "Hello! Please enter your name:"
    onServer $ handshake <.> name

    createGameBtn createGame

    gameList <- onServer getGamesList
    mapM_ (addGameToDOM joinGame) gameList

    return ()

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
