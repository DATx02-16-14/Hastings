-- |Tests functions from "Hastings.ServerUtils".
-- Does not test simple functions such as those running a 'find'
-- or simple 'map's.
module Hastings.ServerUtilsTests where

import Test.QuickCheck
import Control.Concurrent (Chan, newChan)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub)
import Data.ByteString.Char8 (empty)

import LobbyTypes
import Hastings.ServerUtils

-- To run tests in ghci
-- :l test/Hastings/ServerUtilsTests.hs src/Hastings/ServerUtils.hs src/Hastings/Utils.hs src/LobbyTypes.hs test/ArbitraryLobbyTypes.hs

-- Tests

-- |Property that checks that the UUIDs received are the correct ones
prop_getUUIDFromGamesList :: [LobbyGame] -> Property
prop_getUUIDFromGamesList list = not (null list) ==>
  [fst x | x <- list ] == getUUIDFromGamesList list

-- |Property that checks that a player is deleted
prop_deletePlayerFromGame_length :: Int -> LobbyGame -> Property
prop_deletePlayerFromGame_length i g@(_, gameData) = not (null (players gameData)) ==>
  length (players gameData) - 1 == length (players newGameData)
  where
    (_, newGameData) = deletePlayerFromGame i' g

    i' =  abs $ mod i (length $ players gameData)


-- |Property that checks that only the correct one has changed, and all others have the same length.
-- Runs nub on the list of players since sessionID's are meant to be unique
-- Also goes through each LobbyGame to make sure the GameID is unique.
prop_addPlayerToGame_length :: Int -> [LobbyGame] -> Property
prop_addPlayerToGame_length i list = not (null list) ==>
  addPlayerToGamePropTemplate i list fun
  where
    fun :: String -> [LobbyGame] -> [LobbyGame] -> Bool
    fun gameID list newList = all prop $ zip list newList
      where
        prop ((guuid,og), (_,ng)) | guuid == gameID = length (nub $ players og) + 1 == length (players ng)
                                  | otherwise       = length (players og) == length (players ng)

-- |Checks that only one lobbyGame has changed
prop_addPlayerToGame_unique :: Int -> [LobbyGame] -> Property
prop_addPlayerToGame_unique i list = not (null list) ==>
  addPlayerToGamePropTemplate i list (\_ list newList -> 1 >= foldr addIfChanged 0 (zip list newList))
  where
    addIfChanged ((guuid,og), (_,ng)) | length (players og) == length (players ng) = (+ 0)
                                      | otherwise                                  = (+ 1)
-- |Template for tests with addPlayerToGame
-- Requires a function that wants the GameID (of the game that was changed), and two lists of games
-- , one unchanged and one where a player has been added.
addPlayerToGamePropTemplate :: Int -- ^ This 'Int' corresponds to which 'LobbyGame' to choose
  -> [LobbyGame] -- ^The list to test
  -> ( String -- ^GameID of the choosen game
    -> [LobbyGame] -- ^The originial list of 'LobbyGame's
    -> [LobbyGame] -- ^The list of 'LobbyGame's where a player has been adde
    -> Bool) -- ^The function that determines if the test succeeds
  -> Bool
addPlayerToGamePropTemplate i list fun = fun gameID list' newList
  where
    list' = zipWith newLobbyGame list [0..]
    newLobbyGame (_, gameData) i = (show i, gameData)

    i' = abs $ mod i (length list')

    lobbyChan = unsafePerformIO newChan
    client = ClientEntry 18446744073709551615 "new client" [] lobbyChan
    gameID = fst $ list' !! i'
    newList = addPlayerToGame client gameID list'

    playersList list = players $ snd $ list !! i'

  -- other prop: check that it was not possible to join if max has been reached

-- |Checks that findGameWithID finds the correct game
-- I.e. that the gameID is the same.
prop_findGameWithID :: Int -> [LobbyGame] -> Property
prop_findGameWithID i list = not (null list) ==>
  case findGameWithID gameID list' of
    Nothing         -> False
    Just (guuid, _) -> gameID == guuid
  where
    i' = abs $ mod i (length list')
    gameID = fst $ list' !! i'

    list' = zipWith newLobbyGame list [0..]
    newLobbyGame (_, gameData) i = (show i, gameData)

-- |Checks that this function always returns correct results
-- I.e. given a SessionID that corresponds to the last player it returns 'True'
-- and given any other SessionID it returns 'False'.
-- The test fails if the SessionID's of the clients are not unique
prop_isOwnerOfGame :: Int -- ^This 'Int' correseponds to which LobbyGame to choose
  -> Int -- ^This 'Int' corresponds to which player to choose
  -> [LobbyGame] -- ^The list of 'LobbyGame's to test. Is modified to make sure gameID is unique
  -> Property
prop_isOwnerOfGame i j list = not (null list) && not (null playersList)==>
  if isOwnerOfGame sid list'
    then j' == (length playersList - 1)
    else j' /= (length playersList - 1)
  where
    i' = abs $ mod i (length list')
    j' = abs $ mod j (length playersList)
    sid = sessionID $ playersList !! j'

    playersList = players $ snd $ list' !! i'

    list' = zipWith newLobbyGame list [0..]
    newLobbyGame (_, gameData) i = (show i, gameData)
