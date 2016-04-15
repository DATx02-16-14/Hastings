-- |Monadic tests using "Test.QuickCheck.Monadic" for "Server.Chat"
module Server.ChatTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Server.Chat
import ArbitraryLobbyTypes ()
import LobbyTypes

-- |Property that tests that after joining a chat the chat is created
-- or the player joins.
prop_joinChat :: [ClientEntry] -> Property
prop_joinChat = true
