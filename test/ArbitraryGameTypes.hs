module ArbitraryGameTypes where

import Test.QuickCheck (Arbitrary, Gen, elements, arbitrary)
import Data.ByteString.Char8 (empty)

import Hastings.Database.Fields

-- |Arbitrary GameData
-- Limits the maxAmountOfPlayers to 26.
instance Arbitrary Game where
  arbitrary = do
    gameId <- elements [1..184467] -- GameId should be unique, increasing this number will make some props very slow
    name <- arbitrary :: Gen String
    maxPlayers <- arbitrary :: Gen Int
    owner <- elements [1..184467] -- GameId should be unique, increasing this number will make some props very slow
    let maxPlayers' = 1 + (abs.flip mod 25) maxPlayers -- Reasonable amount of max players
    playersNum <- arbitrary :: Gen Int
    let playersNum' = (abs.flip mod maxPlayers') playersNum -- No more players than max number
    return $ Game (show gameId) name maxPlayers' owner empty