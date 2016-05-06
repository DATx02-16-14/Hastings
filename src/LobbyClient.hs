-- |Module for all of the client only code
module LobbyClient where
import Haste.App
import Haste.DOM
import Haste.Concurrent

import Data.Maybe
import Control.Monad (when)

import System.Random

import Views.Common
import Views.Lobby
import Views.Game
import LobbyAPI
import GameAPI
import LobbyTypes
import Views.Chat

-- |Main mehtod for the client.
clientMain :: LobbyAPI -> GameAPI -> Client ()
clientMain lapi gapi = do
  --name <- prompt "Hello! Please enter your name:"
  onServer $ connect lapi <.> "testName"

  initDOM
  createBootstrapTemplate "Hastings"
  createChangeNickNameDOM lapi
  createChatDOM lapi
  createLobbyDOM lapi gapi

  fork $ listenForLobbyChanges lapi gapi
  clientJoinChat lapi "main"
  
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
      playerLeftGameFun
    PlayerJoinedGame -> updatePlayerListGame api
    PlayerLeftGame   -> playerLeftGameFun
    (LobbyError msg) -> showError msg
  listenForLobbyChanges api gapi
  where
    playerLeftGameFun = do
      updatePlayerListGame api
      isOwner <- onServer $ isOwnerOfCurrentGame api
      when isOwner $ do
        deleteGameOwnerDOM
        createGameOwnerDOM api gapi
