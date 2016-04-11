-- |Contains tests for "Server.Lobby" module.
module LobbyTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (readMVar, newMVar)

import qualified Server.Lobby
import ArbitraryLobbyTypes ()
import LobbyTypes

-- To run in ghci
-- :l test/Server/LobbyTest.hs src/Hastings/ServerUtils.hs src/Hastings/Utils.hs src/LobbyTypes.hs test/ArbitraryLobbyTypes.hs src/Server/Lobby.hs src/Server/Game.hs src/Server/Chat.hs src/Hastings/Database/Player.hs

-- |Property for connect that makes sure that after running connect a ClientEntry with
-- sessionID and name is found in the list.
prop_connect :: Name -> [ClientEntry] -> Property
prop_connect playerName list = monadicIO $ do
  sessionIDWord64 <- pick $ elements [184468..282345]
  mVar <- run $ newMVar list
  run $ Server.Lobby.connect mVar playerName sessionIDWord64
  conList <- run $ readMVar mVar
  assert $ any (\c -> sessionIDWord64 == sessionID c && name c == playerName) conList

-- |Property for disconnect that checks that after disconnecting, the cliententry
-- is removed from all games and the list of clients.
prop_disconnect :: Int  -- ^The index of player to remove
                -> [ClientEntry] -> [LobbyGame] -> Property
prop_disconnect i clientList gameList = monadicIO $ do
  pre $ not $ null clientList
  let i' = abs $ mod i $ length clientList
  let client = clientList !! i'
  let sid = sessionID client

  clientMVar <- run $ newMVar clientList
  gameMVar <- run $ newMVar gameList

  run $ Server.Lobby.disconnect clientMVar gameMVar sid

  newClientList <- run $ readMVar clientMVar
  newGameList <- run $ readMVar gameMVar

  assert $
    (all (client /=) newClientList) &&
    (all (\(_,gd) -> all (client /=) $ players gd) newGameList)
