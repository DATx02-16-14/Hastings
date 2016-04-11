-- Contains arbitrary declarations of the Data types in LobbyTypes.
module ArbitraryLobbyTypes where

import Test.QuickCheck
import Haste.App (SessionID)
import Data.Word (Word64)
import Control.Concurrent (Chan, newChan)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Char8 (empty)

import LobbyTypes

-- Arbitrary instances for the Data types

-- |Arbitrary ClientEntry
-- Does not have any chat channels, and the LobbyChannel is created
-- by unsafePerformIO (!)
instance Arbitrary ClientEntry where
  arbitrary = do
    sessionIDWord64 <- elements [1..184467] -- SessionID should be unique, increasing this number will make some props very slow
    clientNr <- arbitrary :: Gen Int
    let chan = unsafePerformIO newChan
    return $ ClientEntry sessionIDWord64 ("ClientEntry " ++ show clientNr) [] chan

-- |Arbitrary GameData
-- Limits the maxAmountOfPlayers to 26.
instance Arbitrary GameData where
  arbitrary = do
    gameNr <- arbitrary :: Gen Int
    maxPlayers <- arbitrary :: Gen Int
    let maxPlayers' = 1 + (abs.flip mod 25) maxPlayers -- Reasonable amount of max players
    playersNum <- arbitrary :: Gen Int
    let playersNum' = (abs.flip mod maxPlayers') playersNum -- No more players than max nuber
    clients <- sequence [ arbitrary | _ <- [1..playersNum']]
    return $ GameData clients ("GameData " ++ show gameNr) maxPlayers' empty
