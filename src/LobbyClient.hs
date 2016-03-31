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
clientMain :: LobbyAPI -> Client ()
clientMain api = do
  name <- prompt "Hello! Please enter your name:"
  onServer $ connect api <.> name

  initDOM
  createBootstrapTemplate "Hastings"
  createChangeNickNameDOM api
  createChatDOM api
  createLobbyDOM api

  fork $ listenForLobbyChanges api
  onServer $ joinChat api <.> "main"
  fork $ listenForChatMessages api "main" chatMessageCallback
  return ()

listenForLobbyChanges :: LobbyAPI -> Client ()
listenForLobbyChanges api = do
  message <- onServer $ readLobbyChannel api
  case message of
    GameNameChange   -> do
      updateGameHeader api
      updateGamesList api
    NickChange       -> do
      updatePlayerList api
      updatePlayerListGame api
    KickedFromGame   -> do
      deleteGameDOM
      createLobbyDOM api
    GameAdded        -> updateGamesList api
    ClientJoined     -> updatePlayerList api
    ClientLeft       -> do
      updatePlayerList api
      updatePlayerListGame api
    PlayerJoinedGame -> updatePlayerListGame api
    (LobbyError msg) -> liftIO $ print msg -- Update with some way to show error to client
  listenForLobbyChanges api
