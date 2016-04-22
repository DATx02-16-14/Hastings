-- |Contains tests for "Server.Game" module.
module Server.GameTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (newMVar)
import Haste.App (SessionID)
import qualified Database.Esqueleto as Esql

import qualified Server.Game
import ArbitraryGameTypes ()
import LobbyTypes
import Hastings.Database.Common (migrateDatabase, clearDatabase)
import qualified Hastings.Database.Player as PlayerDB
import qualified Hastings.Database.Game as GameDB
import qualified Hastings.Database.Fields as Fields


-- | Run database setup functions that makes sure the database
--   table is up to date.
preProp :: IO ()
preProp = do
  migrateDatabase
  PlayerDB.clearOnlinePlayers
  clearDatabase

-- | Remove all data in DB tables.
postProp :: IO ()
postProp = clearDatabase



-- |Property that makes sure that after calling leaveGame
-- there is no player with that sessionID left.
prop_leaveGame :: Fields.Game -> [ClientEntry] -> Property
prop_leaveGame game clientList = monadicIO $ do
  pre $ not $ null clientList

  let sid = sessionID $ head clientList
  let playerName = name $ head clientList

  run preProp

  --Setup test preconditions
  clientMVar <- run $ newMVar clientList

  run $ PlayerDB.saveOnlinePlayer playerName sid
  gameKey <- run $ GameDB.saveGame (Fields.gameUuid game)
                                   (Fields.gameName game)
                                   (Fields.gameMaxAmountOfPlayers game)
                                   (Fields.gameOwner game)
                                   (Fields.gamePassword game)

  -- Add all players to the game
  mapM_ (run . (`GameDB.addPlayerToGame` gameKey) . sessionID) clientList

  --Run test operations.
  playersInGameBefore <- run $ GameDB.retrievePlayersInGame gameKey
  run $ Server.Game.leaveGame clientMVar sid
  playersInGame <- run $ GameDB.retrievePlayersInGame gameKey

  assert $
    --The player with the correct username has been removed.
    playerName `notElem` map (Fields.playerUserName . Esql.entityVal) playersInGame &&
    --Only one player has been removed
    length playersInGameBefore - length playersInGame == 1

  --Cleanup test
  run postProp
