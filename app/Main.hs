{-# LANGUAGE CPP #-}
-- |Main module of the Lobby
module Main
    where
import Haste.App
import Haste.App.Concurrent
import Views.Lobby
import qualified Control.Concurrent as CC
import LobbyAPI
import LobbyTypes
import GameAPI

#ifdef __HASTE__
import LobbyClient
#define disconnect(x) (\_ -> return ())
#else
import LobbyServer
#define clientMain (\_ _ -> return ())
#endif

-- |Main method and entry point for the program
main :: IO ()
main = runApp defaultConfig $ do
  playersList <- liftServerIO $ CC.newMVar []
  gamesList <- liftServerIO $ CC.newMVar []
  chatList <- liftServerIO $ CC.newMVar []

  let serverState = (playersList, gamesList, chatList)

  onSessionEnd $ disconnect(serverState)
  api <- newLobbyAPI (playersList, gamesList, chatList)
  runClient $ clientMain api newGameAPI
