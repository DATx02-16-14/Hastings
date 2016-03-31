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
  createLobbyDOM api newGameAPI


  fork $ listenForLobbyChanges api newGameAPI
  onServer $ joinChat api <.> "main"
  fork $ listenForChatMessages api "main" chatMessageCallback
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
  listenForLobbyChanges api gapi
