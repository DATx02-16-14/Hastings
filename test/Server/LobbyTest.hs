-- |Contains tests for "Server.Lobby" module.
module LobbyTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (readMVar, newMVar)

import Server.Lobby
import ArbitraryLobbyTypes ()
import LobbyTypes

-- To run in ghci
-- :l test/Server/LobbyTest.hs src/Hastings/ServerUtils.hs src/Hastings/Utils.hs src/LobbyTypes.hs test/ArbitraryLobbyTypes.hs src/Server/Lobby.hs src/Server/Game.hs src/Server/Chat.hs src/Hastings/Database/Player.hs

-- |Test for connect that makes sure that after running connect a sessionID
-- and name is found in the list.
prop_connect :: Name -> [ClientEntry] -> Property
prop_connect playerName list = monadicIO $ do
  pre $ not $ null list
  sessionIDWord64 <- pick $ elements [184468..282345]
  mVar <- run $ newMVar list
  run $ connect mVar playerName sessionIDWord64
  conList <- run $ readMVar mVar
  assert $ any (\c -> sessionIDWord64 == sessionID c && name c == playerName) conList
