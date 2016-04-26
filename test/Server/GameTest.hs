-- |Contains tests for "Server.Game" module.
module Server.GameTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (newMVar, readMVar)
import Haste.App (SessionID)
import Data.Maybe (isJust, fromJust)
import Data.List (nubBy)
import Data.Function (on)
import Data.ByteString.Char8 (ByteString, empty, pack, unpack)
import Crypto.PasswordStore (makePassword, verifyPassword)
import qualified Database.Esqueleto as Esql

import qualified Server.Game
import ArbitraryGameTypes ()
import LobbyTypes
import Hastings.Database.Common (migrateDatabase, clearDatabase)
import qualified Hastings.Database.Player as PlayerDB
import qualified Hastings.Database.Game as GameDB
import qualified Hastings.Database.Fields as Fields

type GameKey = Esql.Key Fields.Game

-- | Run database setup functions that makes sure the database
--   table is up to date.
preProp :: IO ()
preProp = do
  migrateDatabase
  clearDatabase

-- | Remove all data in DB tables.
postProp :: IO ()
postProp = clearDatabase


-- | Takes a variable of the type Game and saves it to the database.
saveGameToDB :: Fields.Game -> IO (Esql.Key Fields.Game)
saveGameToDB game = GameDB.saveGame (Fields.gameUuid game)
                                    (Fields.gameName game)
                                    (Fields.gameMaxAmountOfPlayers game)
                                    (Fields.gameOwner game)
                                    (Fields.gamePassword game)
{-|
 | Wrapper function that runs some common checks and initialisations before running a game test.
 'gameTestWrapper' will do the following things before running the test.

  * Remove all sessionIds and names that are not unique from the list of clients.
  * Make sure that there are at least two clients left in that list.
  * Extract the name and sessionID from the first client in the client list.
  * run 'preProp'.
  * Save the player that was extracted from the client list to the OnlinePlayers table.
  * Save the game to the database.

 'gameTestWrapper' will do the following things after running the test.

  * run 'postProp'.
-}
gameTestWrapper :: [ClientEntry]            -- A list of randomly generated ClientEntries.
                -> Fields.Game              -- A randomly generated game.
                -> (SessionID               -- The Session ID of the first ClientEntry in the list of clients.
                   -> Name                  -- The name of the first ClientEntry in the list of clients.
                   -> ConcurrentClientList  -- The list of clients.
                   -> GameKey               -- The key to the game that was saved in the database.
                   -> PropertyM IO a)
                -> Property
gameTestWrapper clientList game testFunction = monadicIO $ do
  --Remove all names and sessionIDs that are not unique.
  let cleanClientList = nubBy ((==) `on` name) $ nubBy ((==) `on` sessionID) clientList
  pre $ length cleanClientList > 1

  let sid = sessionID $ head cleanClientList
  let playerName = name $ head cleanClientList

  run preProp

  --Setup test preconditions
  clientMVar <- run $ newMVar $ tail cleanClientList

  run $ PlayerDB.saveOnlinePlayer playerName sid
  gameKey <- run $ saveGameToDB game

  testFunction sid playerName clientMVar gameKey

  run postProp


-- |Property that makes sure that after calling leaveGame
-- there is no player with that sessionID left.
prop_leaveGame :: [ClientEntry] -> Fields.Game -> Property
prop_leaveGame clientList game = gameTestWrapper clientList game prop_leaveGame'
  where
    prop_leaveGame' :: SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_leaveGame' sid playerName clientMVar gameKey = do
      -- Add all players to the game
      clientList <- run $ readMVar clientMVar
      run $ mapM_ ((`GameDB.addPlayerToGame` gameKey) . sessionID) clientList
      run $ GameDB.addPlayerToGame sid gameKey

      --Run test operations.
      playersInGameBefore <- run $ GameDB.retrievePlayersInGame gameKey
      run $ Server.Game.leaveGame clientMVar sid
      playersInGame <- run $ GameDB.retrievePlayersInGame gameKey
      assert $
        --The player with the correct username has been removed.
        playerName `notElem` map (Fields.playerUserName . Esql.entityVal) playersInGame &&
        --Only one player has been removed
        length playersInGameBefore - length playersInGame == 1

-- |Property that makes sure that after calling joinGame
-- the player has correctly joined the game.
prop_joinGame :: [ClientEntry] -> Fields.Game -> Property
prop_joinGame clientList game = gameTestWrapper clientList game prop_joinGame'
  where
    prop_joinGame' :: SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_joinGame' sid playerName clientMVar gameKey = do
      player <- run $ PlayerDB.retrieveOnlinePlayer sid
      allowedToJoin <- run $ Server.Game.playerJoinGame clientMVar sid (Fields.gameUuid game) ""
      playersInGame <- run $ GameDB.retrievePlayersInGame gameKey
      game <- run $ GameDB.retrieveGameBySid sid

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
prop_findGameNameWithSid :: [ClientEntry] -> Fields.Game -> Property
prop_findGameNameWithSid clientList game = gameTestWrapper clientList game prop_findGameNameWithSid'
  where
    prop_findGameNameWithSid' :: SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_findGameNameWithSid' sid playerName clientMVar gameKey = do
      run $ GameDB.addPlayerToGame sid gameKey
      gameName <- run $ Server.Game.findGameNameWithSid sid
      assert $ gameName == Fields.gameName game

-- |Property that checks that all playerNames are correctly found in a game.
prop_playerNamesInGameWithSid :: [ClientEntry] -> Fields.Game -> Property
prop_playerNamesInGameWithSid clientList' game = gameTestWrapper clientList' game prop_playerNamesInGameWithSid'
    where
      prop_playerNamesInGameWithSid' :: SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
      prop_playerNamesInGameWithSid' sid playerName clientMVar gameKey = do
        -- Add all players to the game
        clientList <- run $ readMVar clientMVar
        run $ mapM_ (\client -> PlayerDB.saveOnlinePlayer (name client) (sessionID client)) clientList
        run $ GameDB.addPlayerToGame sid gameKey
        run $ mapM_ ((`GameDB.addPlayerToGame` gameKey) . sessionID) clientList

        playerNames <- run $ Server.Game.playerNamesInGameWithSid sid

        assert $
          --Add +1 to compensate for adding the player not in the clientList.
          length playerNames == length clientList + 1 &&
          --All names in the clientList should occur in playerNames
          all (\client -> name client `elem` playerNames) clientList &&
          playerName `elem` playerNames

-- |Property that checks that only the correct player has been kicked from a game.
prop_kickPlayerWithSid :: Int -> [ClientEntry] -> Fields.Game -> Property
prop_kickPlayerWithSid index clientList game = gameTestWrapper clientList game (prop_kickPlayerWithSid' index)
  where
    prop_kickPlayerWithSid' :: Int -> SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_kickPlayerWithSid' index sid playerName clientMVar gameKey = do
      clientList <- run $ readMVar clientMVar
      let kickedClient = clientList !! (index `mod` length clientList)

      -- Add all players to the game
      run $ mapM_ (\client -> PlayerDB.saveOnlinePlayer (name client) (sessionID client)) clientList
      run $ mapM_ ((`GameDB.addPlayerToGame` gameKey) . sessionID) clientList
      run $ GameDB.addPlayerToGame sid gameKey

      playersInGameBefore <- run $ GameDB.retrievePlayersInGame gameKey

      run $ Server.Game.kickPlayerWithSid clientMVar sid (name kickedClient)

      playersInGame <- run $ GameDB.retrievePlayersInGame gameKey
      playerNamesInGame <- run $ Server.Game.playerNamesInGameWithSid sid

      assert $
        --Only one player has been kicked.
        length playersInGameBefore - length playersInGame == 1 &&
        --The correct player has been kicked.
        name kickedClient `notElem` playerNamesInGame

-- |Property that checks that the correct game has had its name changed.
prop_changeGameNameWithSid :: Name -> [ClientEntry] -> Fields.Game  -> Property
prop_changeGameNameWithSid newName clientList game = gameTestWrapper clientList game (prop_changeGameNameWithSid' newName)
  where
    prop_changeGameNameWithSid' :: Name -> SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_changeGameNameWithSid' newName sid playerName clientMVar gameKey = do
      pre $ not $ null newName

      run $ GameDB.addPlayerToGame sid gameKey

      let oldName = Fields.gameName game
      run $ Server.Game.changeGameNameWithSid clientMVar sid newName
      newGame <- run $ GameDB.retrieveGameBySid sid

      assert $
        --The game should still exist.
        isJust newGame &&
        --The game has the new name
        (Fields.gameName . Esql.entityVal . fromJust) newGame == newName

-- |Property that checks that changing the max amount of player in a game works
prop_changeMaxNumberOfPlayers :: Int -> [ClientEntry] -> Fields.Game -> Property
prop_changeMaxNumberOfPlayers newAmount clientList game = gameTestWrapper clientList game (prop_changeMaxNumberOfPlayers' newAmount)
  where
    prop_changeMaxNumberOfPlayers' :: Int -> SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_changeMaxNumberOfPlayers' newAmount' sid playerName clientMVar gameKey = do
      let newAmount = newAmount' `mod` 6
      let oldAmount = Fields.gameMaxAmountOfPlayers game

      pre $ oldAmount /= newAmount

      run $ GameDB.addPlayerToGame sid gameKey
      run $ GameDB.setGameOwner gameKey sid
      run $ Server.Game.changeMaxNumberOfPlayers sid newAmount

      newGame <- run $ GameDB.retrieveGameBySid sid

      assert $
        --The game should still exist.
        isJust newGame &&
        --The game has the new max amount of players.
        (Fields.gameMaxAmountOfPlayers . Esql.entityVal . fromJust) newGame == newAmount

-- |Property that checks that the correct password is set on a game.
prop_setPasswordToGame :: String -> [ClientEntry] -> Fields.Game -> Property
prop_setPasswordToGame newPassword clientList game = gameTestWrapper clientList game (prop_setPasswordToGame' newPassword)
  where
    prop_setPasswordToGame' :: String -> SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_setPasswordToGame' newPassword sid playerName clientMVar gameKey = do
      pre $ not $ null newPassword

      run $ GameDB.addPlayerToGame sid gameKey
      run $ GameDB.setGameOwner gameKey sid

      run $ Server.Game.setPasswordToGame clientMVar sid newPassword
      newGame <- run $ GameDB.retrieveGameBySid sid

      assert $
        --The game should still exist.
        isJust newGame &&
        --The game has the correct password
        verifyPassword (pack newPassword) ((Fields.gamePassword . Esql.entityVal . fromJust) newGame)

-- |Property that checks that the game is password protected after a password is set.o
prop_isGamePasswordProtected :: String -> [ClientEntry] -> Fields.Game -> Property
prop_isGamePasswordProtected newPassword clientList game = gameTestWrapper clientList game (prop_isGamePasswordProtected' newPassword)
  where
    prop_isGamePasswordProtected' :: String -> SessionID -> Name -> ConcurrentClientList -> GameKey -> PropertyM IO ()
    prop_isGamePasswordProtected' newPassword sid playerName clientMVar gameKey = do
      pre $ not $ null newPassword

      run $ GameDB.addPlayerToGame sid gameKey
      run $ GameDB.setGameOwner gameKey sid

      ispasswordProtectedBefore <- run $ Server.Game.isGamePasswordProtected (Fields.gameUuid game)

      run $ Server.Game.setPasswordToGame clientMVar sid newPassword

      isPasswordProtected <- run $ Server.Game.isGamePasswordProtected (Fields.gameUuid game)

      assert $
        --The game shouldn't be password protected before.
        not ispasswordProtectedBefore &&
        --The game should be password protected after.
        isPasswordProtected
