{-# LANGUAGE CPP #-}
-- |Main module of the Lobby
module Main
    where
import Haste.App
import Haste.App.Standalone
import Haste.App.Concurrent
import Views.Lobby
import qualified Control.Concurrent as CC
import Haste.Events
import Haste.DOM
import Hastings.Utils
import Data.Maybe
import LobbyAPI
import LobbyTypes
import qualified Chat as Chat

#ifdef __HASTE__
import LobbyClient
#define disconnect(x) (\_ -> return ())
#else
import LobbyServer
#define clientMain (\_ -> return ())
#endif

-- |Main method and entry point for the program
main :: IO ()
main = runStandaloneApp $ do
  playersList <- liftServerIO $ CC.newMVar []
  gamesList <- liftServerIO $ CC.newMVar []
  chatList <- liftServerIO $ CC.newMVar $ (Chat.createNewChatRoom "main") : []

  let serverState = (playersList, gamesList, chatList)

  onSessionEnd $ disconnect(serverState)
  api <- newLobbyAPI (playersList, gamesList, chatList)
  runClient $ clientMain api

