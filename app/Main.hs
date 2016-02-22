{-# LANGUAGE CPP #-}
module Main
    where
import Haste.App
import Haste.App.Standalone
import Lobby
import qualified Control.Concurrent as CC
import Haste.Events
import Haste.DOM
import Hastings.Utils
import Data.Maybe
import LobbyAPI
#ifdef __HASTE__
#define closeConnection(x) (\_ -> return ())
import LobbyClient
#else
import LobbyServer
#define clientMain (\_ -> return ())
#endif

main :: IO ()
main = runStandaloneApp $ do
  playersList <- liftServerIO $ CC.newMVar []
  gamesList <- liftServerIO $ CC.newMVar []
  onSessionEnd $ closeConnection(playersList)
  api <- newLobbyAPI playersList gamesList
  runClient $ clientMain api
