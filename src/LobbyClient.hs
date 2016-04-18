-- |Module for all of the client only code
module LobbyClient where
import Haste.App
import Haste.DOM
import Haste.Concurrent

import Data.Maybe
import Control.Monad (when)
import qualified Control.Concurrent as CC

import Views.Common
import Views.Lobby
import Views.Game
import LobbyAPI
import LobbyTypes
import Views.Chat
import ChineseCheckers.ChineseGame

-- |Main mehtod for the client.
clientMain :: LobbyAPI -> Client ()
clientMain lapi = do
  name <- prompt "Hello! Please enter your name:"
  onServer $ connect lapi <.> name

  initDOM
  createBootstrapTemplate "Hastings"
  createChangeNickNameDOM lapi
  createChatDOM lapi
  createLobbyDOM lapi

  fork $ listenForLobbyChanges lapi
  clientJoinChat lapi "main"

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
      playerLeftGameFun
    PlayerJoinedGame -> updatePlayerListGame api
    PlayerLeftGame   -> playerLeftGameFun
    StartGame        -> do
      gameState <- liftIO $ CC.newEmptyMVar
      "centerContent" `withElem` \centerContent -> do
          runGame centerContent gameState ["a","b","c","d"] "a" api
      liftIO $ print "StartGame received"
    (LobbyError msg) -> showError msg
  listenForLobbyChanges api
  where
    playerLeftGameFun = do
      updatePlayerListGame api
      isOwner <- onServer $ isOwnerOfCurrentGame api
      when isOwner $ do
        deleteGameOwnerDOM
        createGameOwnerDOM api
