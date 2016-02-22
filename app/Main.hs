{-# LANGUAGE CPP #-}
module Main
    where
import Haste.App
import Haste.App.Standalone
import Haste.App.Concurrent
import Lobby
import qualified Control.Concurrent as CC
import Haste.Events
import Haste.DOM
import Hastings.Utils

import Data.Maybe
import LobbyAPI
#ifdef __HASTE__
#define disconnectPlayerFromLobby(x) (\_ -> return ())
#define disconnectPlayerFromGame(x) (\_ -> return())
import LobbyClient
#else
import LobbyServer
#define clientMain (\_ -> return ())
#endif

main :: IO ()
main = runStandaloneApp $ do
  playersList <- liftServerIO $ CC.newMVar []
  gamesList <- liftServerIO $ CC.newMVar []

  onSessionEnd $ disconnectPlayerFromLobby(playersList)
  onSessionEnd $ disconnectPlayerFromGame(gamesList)
  api <- newLobbyAPI playersList gamesList
  runClient $ clientMain api
