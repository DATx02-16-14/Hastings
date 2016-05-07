-- |Monadic tests using "Test.QuickCheck.Monadic" for "Server.Chat"
module Server.ChatTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent (newMVar, readMVar, newChan, readChan, dupChan)

import qualified Server.Chat
import ArbitraryLobbyTypes ()
import LobbyTypes
import ChineseCheckers.Table


-- stack ghci: :l test/Server/ChatTest.hs test/ArbitraryLobbyTypes.hs

-- |Property that tests that after joining a chat the chat is created
-- or the player has that chat in it's list
prop_joinChat :: Int -> Int -> [String] -> [ClientEntry] -> Property
prop_joinChat randomClientIndex randomChatIndex chatNameList clientList = monadicIO $ do
  pre $ not $ null clientList
  pre $ not $ null chatNameList
  let randomClientIndex' = abs $ randomClientIndex `mod` length clientList
  let randomChatIndex' = abs $ randomChatIndex `mod` 1 + length chatNameList
  let chat = ("new chat" : chatNameList) !! randomChatIndex'
  let client = clientList !! randomClientIndex'
  let sid = sessionID client
  mVarClients <- run $ newMVar clientList
  chatList <- run $ makeChatsOfStrings chatNameList
  mVarChats <- run $ newMVar chatList

  run $ Server.Chat.joinChat mVarClients mVarChats sid chat

  newClientList <- run $ readMVar mVarClients
  newChatList <- run $ readMVar mVarChats
  let newClient = newClientList !! randomClientIndex'
  let clientChats = chats newClient

  assert $
    chat `elem` map fst clientChats &&
    chat `elem` map fst newChatList

-- |Property for leaveChat, tests that the chat is no longer in the players
-- list.
prop_leaveChat :: Int -> Int -> [ClientEntry] -> Property
prop_leaveChat randomClientIndex randomChatIndex clientList = monadicIO $ do
  pre $ not $ null clientList
  let randomClientIndex' = abs $ randomClientIndex `mod` length clientList
  let client = clientList !! randomClientIndex'
  let randomChatIndex' = abs $ randomChatIndex `mod` length (chats client)
  let chatName = fst $ chats client !! randomChatIndex
  let sid = sessionID client

  mVarClients <- run $ newMVar clientList
  run $ Server.Chat.leaveChat mVarClients sid chatName

  newClientList <- run $ readMVar mVarClients
  let newClient = newClientList !! randomClientIndex'
  let clientChats = chats newClient

  assert $ chatName `notElem` map fst clientChats

-- |Property that tests that if a client has joined a chat
-- it receives messages sent by sendChatMessage
prop_sendChatMessage :: Int -> Int -> [String] -> [ClientEntry] -> Property
prop_sendChatMessage randomClientIndex randomChatIndex chatNameList clientList = monadicIO $ do
  pre $ not $ null chatNameList
  pre $ not $ null clientList
  let randomClientIndex' = abs $ randomClientIndex `mod` length chatNameList
  let randomChatIndex' = abs $ randomChatIndex `mod` length clientList
  -- pick a subset of the clients
  let (clientHead, clients) = splitAt randomChatIndex' clientList
  cLobbyChan <- run newChan
  cGameChan <- run newChan
  let c = ClientEntry 123456654321 "Sending client" [] cLobbyChan cGameChan
  chatList <- run $ makeChatsOfStrings $ makeUnique chatNameList
  let chat@(chatName, chatChan) = chatList !! randomClientIndex'
  mVarChats <- run $ newMVar chatList

  -- Add the chat to the clients before creating MVar
  clientsWithChat <- run $ mapM (\cl -> do
      dChan <- dupChan chatChan
      let newChat = (chatName, dChan)
      return $ cl {chats = newChat : chats cl}) (c:clients)
  mVarClients <- run $ newMVar $ clientHead ++ clientsWithChat

  -- send message to all
  let message = ChatMessage (name c) "prop"
  run $ Server.Chat.sendChatMessage mVarClients mVarChats (sessionID c) chatName message

  newClients <- run $ readMVar mVarClients
  let (_, newClientTail) = splitAt randomChatIndex' newClients

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
makeChatsOfStrings = mapM (\n -> do
    chan <- newChan
    return (n, chan))
