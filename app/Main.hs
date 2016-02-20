module Main
    where
import Haste.App
import Haste.App.Standalone
import Lobby
import qualified LobbyServer as Server
import LobbyTypes
import qualified Control.Concurrent as CC
import Haste.Events
import Haste.DOM
import Hastings.Utils
import Data.Maybe

main :: IO ()
main = runStandaloneApp $ do
  playersList <- liftServerIO $ CC.newMVar []
  gamesList <- liftServerIO $ CC.newMVar []

  handshake <- remote $ Server.handshake playersList
  createGame <- remote $ Server.createGame gamesList playersList
  getGamesList <- remote $ Server.getGamesList gamesList
  joinGame <- remote $ Server.playerJoinGame playersList gamesList
  findPlayersInGame <- remote $ Server.playerNamesInGame gamesList
  onSessionEnd $ Server.closeConnection playersList

  runClient $ do
    liftIO createLobbyDOM

    name <- prompt "Hello! Please enter your name:"
    onServer $ handshake <.> name

    createGameBtn (createGame) (findPlayersInGame)

    gameList <- onServer getGamesList
    mapM_ (addGameToDOM joinGame) gameList

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
