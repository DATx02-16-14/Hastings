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
import Data.Maybe

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
    liftIO initDOM
    liftIO createLobbyDOM

    gameList <- onServer getGamesList
    mapM_ (addGame joinGame) gameList

    createGameBtn createGame

    name <- prompt "Hello! Please enter your name:"
    onServer $ handshake <.> name

    playerDiv <- elemById "playerList"
    fork $ listenForChanges getPlayerList addPlayerToPlayerlist 1000 $ fromJust playerDiv

    return ()
