-- |Monadic tests using "Test.QuickCheck.Monadic" for "Server.Chat"
module Server.ChatTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (newMVar, readMVar, newChan)

import qualified Server.Chat
import ArbitraryLobbyTypes ()
import LobbyTypes


-- stack ghci: :l test/Server/ChatTest.hs test/ArbitraryLobbyTypes.hs

-- |Property that tests that after joining a chat the chat is created
-- or the player has that chat in it's list
prop_joinChat :: Int -> [String] -> [ClientEntry] -> Property
prop_joinChat i chatNameList clientList = monadicIO $ do
  pre $ not $ null clientList
  pre $ not $ null chatNameList
  let i' = abs $ mod i $ length clientList
  let i'' = abs $ mod i $ 1 + length chatNameList
  let chat = ("new chat" : chatNameList) !! i''
  let client = clientList !! i'
  let sid = sessionID client
  mVarClients <- run $ newMVar clientList
  chatList <- run $ mapM (\n -> do
    chan <- newChan
    return (n, chan)) (chatNameList)
  mVarChats <- run $ newMVar chatList
  run $ Server.Chat.joinChat mVarClients mVarChats sid chat

  newClientList <- run $ readMVar mVarClients
  newChatList <- run $ readMVar mVarChats
  let newClient = newClientList !! i'
  let clientChats = chats newClient

  assert $
    any ((chat == ). fst) clientChats &&
    any ((chat == ). fst) newChatList
