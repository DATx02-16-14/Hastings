-- |Module for all of the client only code
module LobbyClient where
import Lobby
import Haste.App
import LobbyAPI
import Haste.DOM
import Haste.Concurrent
import Data.Maybe

-- |Main mehtod for the client.
clientMain :: LobbyAPI -> Client ()
clientMain api = do
  liftIO createLobbyDOM

  name <- prompt "Hello! Please enter your name:"
  onServer $ connect api <.> name

  createGameBtn api

  gameList <- onServer $ getGamesList api
  mapM_ (addGame api) gameList

  playerDiv <- elemById "playerList"
  fork $ listenForChanges (getPlayerNameList api) addPlayerToPlayerlist 1000 $ fromJust playerDiv


  return ()
