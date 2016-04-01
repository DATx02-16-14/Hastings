-- |Module for all of the client only code
module LobbyClient where
import Views.Common
import Views.Lobby
import Views.Game
import Haste.App
import LobbyAPI
import Haste.DOM
import Haste.Concurrent
import Data.Maybe
import GameAPI
import LobbyTypes
import Views.Chat

-- |Main mehtod for the client.
clientMain :: LobbyAPI -> GameAPI -> Client ()
clientMain lapi gapi = do
  name <- prompt "Hello! Please enter your name:"
  onServer $ connect lapi <.> name

  initDOM
  createBootstrapTemplate "Hastings"
  createChangeNickNameDOM lapi
  createChatDOM lapi
  createLobbyDOM lapi gapi


  fork $ listenForLobbyChanges lapi gapi
  onServer $ joinChat lapi <.> "main"
  fork $ listenForChatMessages lapi "main" chatMessageCallback
  return ()

listenForLobbyChanges :: LobbyAPI -> GameAPI -> Client ()
listenForLobbyChanges api gapi = do
  message <- onServer $ readLobbyChannel api
  case message of
    GameNameChange   -> do
      updateGameHeader api
      updateGamesList api gapi
    NickChange       -> do
      updatePlayerList api
      updatePlayerListGame api
    KickedFromGame   -> do
      deleteGameDOM
      createLobbyDOM api gapi
    GameAdded        -> updateGamesList api gapi
    ClientJoined     -> updatePlayerList api
    ClientLeft       -> do
      updatePlayerList api
      updatePlayerListGame api
    PlayerJoinedGame -> updatePlayerListGame api
    (LobbyError msg) -> liftIO $ print msg -- Update with some way to show error to client
  listenForLobbyChanges api gapi
