-- |Contains arbitrary declarations of the Data types in LobbyTypes.
module ArbitraryLobbyTypes where

import Test.QuickCheck
import Haste.App (SessionID)
import Data.Word (Word64)
import Control.Concurrent (Chan, newChan)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Char8 (empty)

import LobbyTypes
import ChineseCheckers.Table

-- Arbitrary instances for the Data types

-- |Arbitrary ClientEntry
-- Does not have any chat channels, and the LobbyChannel is created
-- by unsafePerformIO (!)
instance Arbitrary ClientEntry where
  arbitrary = do
    sessionIDWord64 <- elements [1..184467] -- SessionID should be unique, increasing this number will make some props very slow
    clientNr <- arbitrary :: Gen Int
    let lobbyChan = unsafePerformIO newChan
    let gameChan = unsafePerformIO newChan
    return $ ClientEntry sessionIDWord64 ("ClientEntry " ++ show clientNr) [] lobbyChan gameChan
