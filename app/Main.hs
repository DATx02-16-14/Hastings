module Main
    where
import Haste.App
import Haste.App.Standalone
import Haste.App.Concurrent
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
  getPlayerList <- remote $ Server.getConnectedPlayers playersList
  onSessionEnd $ Server.closeConnection playersList

  runClient $ do
    liftIO createLobbyDOM

    name <- prompt "Hello! Please enter your name:"
    onServer $ handshake <.> name

    createGameBtn createGame

    gameList <- onServer getGamesList
    mapM_ (addGame joinGame) gameList

    fork $ updatePlayerList getPlayerList []

    return ()
