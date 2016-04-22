-- |Contains tests for "Server.Lobby" module.
module Server.LobbyTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (readMVar, newMVar)
import Haste.App (SessionID)
import Data.Maybe (isNothing, isJust)

import qualified Server.Lobby
import ArbitraryLobbyTypes ()
import LobbyTypes
import Hastings.Database.Common (migrateDatabase)
import qualified Hastings.Database.Player as PlayerDB


-- To run in ghci
-- :l test/Server/LobbyTest.hs src/Hastings/ServerUtils.hs src/Hastings/Utils.hs src/LobbyTypes.hs test/ArbitraryLobbyTypes.hs src/Server/Lobby.hs src/Server/Game.hs src/Server/Chat.hs src/Hastings/Database/Player.hs

-- | Run database setup functions that makes sure the database
--   table is up to date.
preProp :: IO ()
preProp = do
  migrateDatabase
  PlayerDB.clearOnlinePlayers

-- |Property for connect that makes sure that after running connect a ClientEntry with
-- sessionID and name is found in the list.
prop_connect :: Name -> [ClientEntry] -> Property
prop_connect playerName list = monadicIO $ do
  run preProp
  sessionIDWord64 <- pick $ elements [184468..282345]
  mVar <- run $ newMVar list
  run $ Server.Lobby.connect mVar playerName sessionIDWord64
  conList <- run $ readMVar mVar
  assert $ any (\c -> sessionIDWord64 == sessionID c && name c == playerName) conList

-- |Property for disconnect that checks that after disconnecting, the cliententry
-- is removed from all games and the list of clients.
prop_disconnect :: Int  -- ^The index of player to remove
                -> [ClientEntry] -> Property
prop_disconnect i clientList = monadicIO $ do
  pre $ not $ null clientList
  let i' = abs $ mod i $ length clientList
  let client = clientList !! i'
  let sid = sessionID client
  let testName = "testName5939723012395"
  run preProp

  --Setup test preconditions
  clientMVar <- run $ newMVar clientList
  run $ PlayerDB.saveOnlinePlayer testName sid

  run $ Server.Lobby.disconnect clientMVar sid

  newClientList <- run $ readMVar clientMVar
  player <- run $ PlayerDB.retrieveOnlinePlayer sid

  --Cleanup test data
  run $ PlayerDB.deletePlayer testName

  assert $
    notElem client newClientList &&
    isNothing player

-- |Property for getting the names of connected players.
-- Might seem trivial right now.
prop_getConnectedPlayerNames :: [ClientEntry] -> Property
prop_getConnectedPlayerNames list = monadicIO $ do
  run preProp
  clientMVar <- run $ newMVar list
  nameList <- run $ Server.Lobby.getConnectedPlayerNames clientMVar
  assert $ nameList == map name list

-- |Property that makes sure that after calling changeNickName
-- there is no player with the old name and sessionID left.
prop_changeNickName :: Int  -- ^Index of the player to change nick name
                    -> [ClientEntry] -> Property
prop_changeNickName i clientList = monadicIO $ do
  pre $ not $ null clientList
  let i' = abs $ mod i $ length clientList
  let client = clientList !! i'
  let sid = sessionID client
  let playerName = name client
  let newName = "new name"
  run preProp

  --Setup test preconditions
  run $ PlayerDB.saveOnlinePlayer playerName sid
  clientMVar <- run $ newMVar clientList

  run $ Server.Lobby.changeNickName clientMVar sid newName

  newNamePlayer <- run $ PlayerDB.retrievePlayerByUsername newName
  oldNamePlayer <- run $ PlayerDB.retrievePlayerByUsername playerName

  --Cleanup test data
  run $ PlayerDB.deleteOnlinePlayer sid
  run $ PlayerDB.deletePlayer newName

  assert $
    isJust newNamePlayer &&
    isNothing oldNamePlayer
