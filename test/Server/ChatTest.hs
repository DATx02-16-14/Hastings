-- |Monadic tests using "Test.QuickCheck.Monadic" for "Server.Chat"
module Server.ChatTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (newMVar, readMVar, newChan, readChan, dupChan)

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
  chatList <- run $ makeChatsOfStrings chatNameList
  mVarChats <- run $ newMVar chatList

  run $ Server.Chat.joinChat mVarClients mVarChats sid chat

  newClientList <- run $ readMVar mVarClients
  newChatList <- run $ readMVar mVarChats
  let newClient = newClientList !! i'
  let clientChats = chats newClient

  assert $
    any ((chat == ). fst) clientChats &&
    any ((chat == ). fst) newChatList

-- |Property for leaveChat, tests that the chat is no longer in the players
-- list.
prop_leaveChat :: Int -> [ClientEntry] -> Property
prop_leaveChat i clientList = monadicIO $ do
  pre $ not $ null clientList
  let i' = abs $ mod i $ length clientList
  let client = clientList !! i'
  let i'' = abs $ mod i $ length $ chats client
  let chatName = fst $ chats client !! i''
  let sid = sessionID client

  mVarClients <- run $ newMVar clientList
  run $ Server.Chat.leaveChat mVarClients sid chatName

  newClientList <- run $ readMVar mVarClients
  let newClient = newClientList !! i'
  let clientChats = chats newClient

  assert $ all ((chatName /=) . fst) clientChats

-- |Property that tests that if a client has joined a chat
-- it receives messages sent by sendChatMessage
prop_sendChatMessage :: Int -> Int -> [String] -> [ClientEntry] -> Property
prop_sendChatMessage i j chatNameList clientList = monadicIO $ do
  pre $ not $ null chatNameList
  pre $ not $ null clientList
  let i' = abs $ mod i $ length chatNameList
  let j' = abs $ mod j $ length clientList
  -- pick a subset of the clients
  let (clientHead, clients) = splitAt j' clientList
  cLobbyChan <- run newChan
  let c = ClientEntry 123456654321 "Sending client" [] cLobbyChan
  chatList <- run $ makeChatsOfStrings $ makeUnique chatNameList
  let chat@(chatName, chatChan) = chatList !! i'
  mVarChats <- run $ newMVar chatList

  -- Add the chat to the clients before creating MVar
  clientsWithChat <- run $ mapM (\cl -> do
      dChan <- dupChan chatChan
      let newChat = (chatName, dChan)
      return $ cl {chats = newChat : chats cl}) (c:clients)
  mVarClients <- run $ newMVar $ clientHead ++ clientsWithChat

  -- send message to all
  let message = (ChatMessage (name c) "prop")
  run $ Server.Chat.sendChatMessage mVarClients mVarChats (sessionID c) chatName message

  newClients <- run $ readMVar mVarClients
  let (_, newClientTail) = splitAt j' newClients

  -- collect all received messages
  messages <- run $ mapM (\cl ->
    case lookup chatName $ chats cl of
      Nothing   -> return $ ChatError "Channel not found"
      Just chan -> readChan chan) newClientTail

  -- make sure the messages are the same
  assert $ all (msgProp "prop") messages
  where
    msgProp msg (ChatMessage _ msgContent) = msgContent == msg
    msgProp _   _                          = False

    makeUnique list = [ name ++ show x | (x, name) <- zip [1..] list ]

makeChatsOfStrings :: [String] -> IO [Chat]
makeChatsOfStrings list = mapM (\n -> do
    chan <- newChan
    return (n, chan)) list
