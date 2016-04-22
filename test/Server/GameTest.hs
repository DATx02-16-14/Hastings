-- |Contains tests for "Server.Game" module.
module Server.GameTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (newMVar)
import Haste.App (SessionID)
import Data.Maybe (isJust, fromJust)
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


saveGameToDB :: Fields.Game -> IO (Esql.Key Fields.Game)
saveGameToDB game = GameDB.saveGame (Fields.gameUuid game)
                                    (Fields.gameName game)
                                    (Fields.gameMaxAmountOfPlayers game)
                                    (Fields.gameOwner game)
                                    (Fields.gamePassword game)

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
  gameKey <- run $ saveGameToDB game

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

-- |Property that makes sure that after calling joinGame
-- the player has correctly joined the game.
prop_joinGame :: Fields.Game -> [ClientEntry] -> Property
prop_joinGame game clientList = monadicIO $ do
  pre $ not $ null clientList

  let sid = sessionID $ head clientList
  let playerName = name $ head clientList

  run preProp

  --Setup test preconditions.
  clientMVar <- run $ newMVar clientList

  run $ PlayerDB.saveOnlinePlayer playerName sid
  player <- run $ PlayerDB.retrieveOnlinePlayer sid
  gameKey <- run $ saveGameToDB game

  allowedToJoin <- run $ Server.Game.playerJoinGame clientMVar sid (Fields.gameUuid game) ""
  playersInGame <- run $ GameDB.retrievePlayersInGame gameKey
  game <- run $ GameDB.retrieveGameBySid sid

  --Cleanup test.
  run postProp

  assert $
    --Player was allowed to join.
   allowedToJoin &&
    --Check that both game and player exists(for use of fromJust later).
   isJust player && isJust game &&
   --Check that the player is part of the game.
   (Fields.playerUserName . Esql.entityVal . fromJust) player
      `elem` map (Fields.playerUserName . Esql.entityVal) playersInGame &&
   --Only one player should have joined the game.
   (length playersInGame == 1) &&
   --The current player should be owner they are the only player in the game.
   (Fields.gameOwner . Esql.entityVal . fromJust) game == sid

-- |Property that makes sure a game can be properly created.
prop_createGame :: [ClientEntry] -> Int -> Property
prop_createGame clientList maxPlayers = monadicIO $ do
  pre $
    not (null clientList) &&
    maxPlayers /= 0

  let sid = sessionID $ head clientList
  let playerName = name $ head clientList
  run preProp

  --Setup test preconditions.
  clientMVar <- run $ newMVar clientList

  run $ PlayerDB.saveOnlinePlayer playerName sid
  uuid <- run $ Server.Game.createGame clientMVar sid maxPlayers

  game <- run $ GameDB.retrieveGameBySid sid

  --Cleanup test
  run postProp

  assert $
    --Check that the UUID returned exists.
    isJust uuid &&
    --Check that the game exists in the database.
    isJust game &&
    --Check that the max amount of players is correct.
    (Fields.gameMaxAmountOfPlayers . Esql.entityVal . fromJust) game == maxPlayers

-- |Property that makes sure the game with the correct name is returned.
prop_findGameNameWithID :: Fields.Game -> Property
prop_findGameNameWithID game = monadicIO $ do

  run $ saveGameToDB game

  gameName <- run $ Server.Game.findGameNameWithID $ Fields.gameUuid game

  assert $ gameName == Fields.gameName game
