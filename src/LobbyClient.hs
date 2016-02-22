module LobbyClient where
import Lobby
import Haste.App
import LobbyAPI

clientMain :: LobbyAPI -> Client ()
clientMain api = do
  liftIO createLobbyDOM

  name <- prompt "Hello! Please enter your name:"
  onServer $ handshake api <.> name

  createGameBtn (createGame api) (findPlayersInGame api)

  gameList <- onServer $ getGamesList api
  mapM_ (addGame (joinGame api)) gameList

  return ()
