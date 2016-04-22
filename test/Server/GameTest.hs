-- |Contains tests for "Server.Game" module.
module Server.GameTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (newMVar)
import Haste.App (SessionID)
import Data.Maybe (isJust, fromJust)
import Data.List (nubBy)
import Data.Function (on)
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
  run preProp
  run $ saveGameToDB game

  gameName <- run $ Server.Game.findGameNameWithID $ Fields.gameUuid game

  assert $ gameName == Fields.gameName game
  run postProp

-- |Property that makes sure the game with the correct name is returned.
prop_findGameNameWithSid :: ClientEntry -> Fields.Game -> Property
prop_findGameNameWithSid client game = monadicIO $ do
  run preProp

  gameKey <- run $ saveGameToDB game
  run $ GameDB.addPlayerToGame (sessionID client) gameKey

  gameName <- run $ Server.Game.findGameNameWithSid $ sessionID client

  assert $ gameName == Fields.gameName game
  run postProp

-- |Property that checks that all playerNames are correctly found in a game.
prop_playerNamesInGameWithSid :: [ClientEntry] -> Fields.Game -> Property
prop_playerNamesInGameWithSid clientList game = monadicIO $ do
  pre $ not $ null clientList

  let sid = sessionID $ head clientList
  --Remove sessionIDs that are not unique.
  let cleanClientList = nubBy ((==) `on` sessionID) clientList

  run preProp

  --Setup test preconditions.
  clientMVar <- run $ newMVar clientList
  gameKey <- run $ saveGameToDB game

  -- Add all players to the game
  run $ mapM_ (\client -> PlayerDB.saveOnlinePlayer (name client) (sessionID client)) clientList
  run $ mapM_ ((`GameDB.addPlayerToGame` gameKey) . sessionID) clientList

  playerNames <- run $ Server.Game.playerNamesInGameWithSid sid

  run postProp
  assert $
    length playerNames == length clientList &&
    all (\client -> name client `elem` playerNames) clientList

prop_kickPlayerWithSid :: [ClientEntry] -> Fields.Game -> Int -> Property
prop_kickPlayerWithSid clientList game index = monadicIO $ do
  pre $ not $ null clientList

  --Remove names that are not unique
  let cleanClientList = nubBy ((==) `on` name) clientList

  let sid = sessionID $ head cleanClientList
  let kickedClient = cleanClientList !! (index `mod` length cleanClientList)

  run preProp

  --Setup test preconditions.
  clientMVar <- run $ newMVar cleanClientList
  gameKey <- run $ saveGameToDB game

  -- Add all players to the game
  run $ mapM_ (\client -> PlayerDB.saveOnlinePlayer (name client) (sessionID client)) cleanClientList
  run $ mapM_ ((`GameDB.addPlayerToGame` gameKey) . sessionID) cleanClientList

  playersInGameBefore <- run $ GameDB.retrievePlayersInGame gameKey

  run $ Server.Game.kickPlayerWithSid clientMVar sid (name kickedClient)

  playersInGame <- run $ GameDB.retrievePlayersInGame gameKey
  playerNamesInGame <- run $ Server.Game.playerNamesInGameWithSid sid

  run postProp

  assert $
    --Only one player has been kicked.
    length playersInGameBefore - length playersInGame == 1 &&
    --The correct player has been kicked.
    name kickedClient `notElem` playerNamesInGame

-- |Property that checks that the correct game has had its name changed.
prop_changeGameNameWithSid :: [ClientEntry] -> Fields.Game -> Name -> Property
prop_changeGameNameWithSid clientList game newName = monadicIO $ do
  pre $
    not (null clientList) &&
    not (null newName)

  let sid = sessionID $ head clientList
  let playerName = name $ head clientList

  run preProp

  --Setup test preconditions.
  clientMVar <- run $ newMVar clientList
  gameKey <- run $ saveGameToDB game

  run $ PlayerDB.saveOnlinePlayer playerName sid
  run $ GameDB.addPlayerToGame sid gameKey

  let oldName = Fields.gameName game

  run $ Server.Game.changeGameNameWithSid clientMVar sid newName

  newGame <- run $ GameDB.retrieveGameBySid sid

  run postProp

  assert $
    --The game should still exist.
    isJust newGame &&
    --The game has the new name
    (Fields.gameName . Esql.entityVal . fromJust) newGame == newName

-- |Property that checks that changing the max amount of player in a game works
prop_changeMaxNumberOfPlayers :: [ClientEntry] -> Fields.Game -> Int -> Property
prop_changeMaxNumberOfPlayers clientList game newAmount' = monadicIO $ do
  let newAmount = newAmount' `mod` 6
  let oldAmount = Fields.gameMaxAmountOfPlayers game

  pre $
    not (null clientList) &&
    oldAmount /= newAmount

  let sid = sessionID $ head clientList
  let playerName = name $ head clientList

  run preProp

  --Setup test preconditions.
  gameKey <- run $ saveGameToDB game

  run $ PlayerDB.saveOnlinePlayer playerName sid
  run $ GameDB.addPlayerToGame sid gameKey
  run $ GameDB.setGameOwner gameKey sid

  run $ Server.Game.changeMaxNumberOfPlayers sid newAmount

  newGame <- run $ GameDB.retrieveGameBySid sid

  run postProp

  assert $
    --The game should still exist.
    isJust newGame &&
    --The game has the new max amount of players.
    (Fields.gameMaxAmountOfPlayers . Esql.entityVal . fromJust) newGame == newAmount
