module Server.Chat where

import Control.Concurrent (modifyMVar_, newChan, readMVar, writeChan, dupChan, readChan)
import Haste.App (SessionID)
import Data.List (lookup, deleteBy)
import Data.Maybe (isNothing, fromJust)

import Hastings.ServerUtils
import LobbyTypes
import Hastings.Utils

-- |Sends a server notification to all chats the client has joined
notifyClientChats :: ConcurrentClientList -> SessionID -> String -> IO ()
notifyClientChats mVarClients sid notification = do
  clientList <- readMVar mVarClients
  maybe
    (print "notifyClientChats > Could not find sid in connected clients")
    (mapM_ (flip writeChan (ChatMessage "SERVER" notification) . snd) . chats)
    $ sid `lookupClientEntry` clientList

-- |Called by client to join a chat
joinChat :: ConcurrentClientList -> ConcurrentChatList -> SessionID -> String -> IO ()
joinChat mVarClients mVarChats sid chatName = do
  chatList <- readMVar mVarChats
  if isNothing $ chatName `lookup` chatList
    then do
      newChatChannel <- newChan
      modifyMVar_ mVarChats $ \chatList ->
        return $ (chatName, newChatChannel) : chatList
      addChannelToClient sid mVarChats mVarClients
      announceChatJoin mVarClients mVarChats sid chatName
    else do
      addChannelToClient sid mVarChats mVarClients
      announceChatJoin mVarClients mVarChats sid chatName

  where
    addChannelToClient sid mVarChats mVarClients = do
      chatList <- readMVar mVarChats
      let chan = fromJust $ chatName `lookup` chatList
      clientChan <- dupChan chan
      let newChat = (chatName, clientChan)
      modifyMVar_ mVarClients $ \clients ->
        return $ updateListElem (addChatToClient newChat) ((sid ==) . sessionID) clients

    addChatToClient :: Chat -> ClientEntry -> ClientEntry
    addChatToClient chat client | chatName `elem` map fst (chats client) = client
                     | otherwise = client {chats = chat : chats client}

-- | Sends a ChatAnnounceJoin to all clients present in the channel.
-- | String is the name of the channel joined
announceChatJoin :: ConcurrentClientList -> ConcurrentChatList -> SessionID -> String -> IO ()
announceChatJoin mVarClients mVarChats sid chatName = do
  chatList <- readMVar mVarChats
  clientList <- readMVar mVarClients
  case chatName `lookup` chatList of
    Nothing      -> return ()
    Just channel ->
      maybe
        (return ())
        (writeChan channel . ChatAnnounceJoin . name)
        $ sid `lookupClientEntry` clientList

-- | Called by client to leave the named Chat
-- | String is the name of the chat to be left
leaveChat :: ConcurrentClientList -> SessionID -> String -> IO ()
leaveChat mVarClients sid chatName = do
  clientList <- readMVar mVarClients
  case sid `lookupClientEntry` clientList of
    Nothing     -> return ()
    Just client ->
      case chatName `lookup` chats client of
        Nothing      -> return ()
        Just channel -> do
          writeChan channel $ ChatAnnounceLeave $ name client
          modifyMVar_ mVarClients $ \clientList ->
            return $ updateListElem
              (\c -> c {chats =
                deleteBy (\(cName1,_) (cName2,_) -> cName1 == cName2) (chatName, channel) $ chats client
              })
              ((sessionID client ==) . sessionID)
              clientList

-- |Called by a client to read its various chat channels
readChatChannel :: ConcurrentClientList -> SessionID -> String ->  IO ChatMessage
readChatChannel mVarClients sid chatName = do
  clientList <- readMVar mVarClients
  case sid `lookupClientEntry` clientList of
    Nothing     -> return $ ChatError "Couldn't find clients sessionID in remotes client list, try reconnecting."
    Just client ->
      case chatName `lookup` chats client of
        Nothing          -> return $ ChatError "Couldn't find chat in clients currently joined chats. join chat before trying to read from it"
        Just chatChannel -> readChan chatChannel

-- |Called by a client to get its name based on sessionID
sendChatMessage :: ConcurrentClientList -> ConcurrentChatList -> SessionID -> String -> ChatMessage -> IO ()
sendChatMessage mVarClients mVarChats sid chatName chatMessage = do
  chatList <- readMVar mVarChats
  clientList <- readMVar mVarClients
  case chatName `lookup` chatList of
    Nothing   -> return ()
    Just chatChannel ->
      case sid `lookupClientEntry` clientList of
        Nothing     -> return()
        Just client -> do
          let msg = chatMessage {from = name client}
          writeChan chatChannel $ mapMessage msg client

  return ()
    where
      mapMessage msg1 client = case msg1 of
        (ChatMessage from content) -> msg1
        ChatJoin                   -> ChatAnnounceJoin  (name client)
        ChatLeave                  -> ChatAnnounceLeave (name client)

-- | Return list of chatnames which the client have joined
getJoinedChats :: ConcurrentClientList -> SessionID -> IO [String]
getJoinedChats mVarClients sid = do
  clientList <- readMVar mVarClients
  case sid `lookupClientEntry` clientList of
    Nothing     -> do
      print "getJoinedChats > Error: expected client not found."
      return []
    Just client ->
      return $ map fst $ chats client

-- | Return list of all chatnames
getChats :: ConcurrentChatList -> IO [String]
getChats mVarChats = do
  chatList <- readMVar mVarChats
  return $ map fst chatList
