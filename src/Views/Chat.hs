module Views.Chat
    (
    listenForChatMessages,
    chatMessageCallback,
    createChatDOM,
    clientJoinChat
    ) where

import Haste.App
import Haste
import Haste.Events
import Haste.DOM
import Haste.App.Concurrent
import LobbyTypes
import LobbyAPI
import Control.Monad (unless)

-- | Chat dom creation.
createChatDOM :: LobbyAPI -> Elem -> Client ()
createChatDOM api parentDiv = do

  br <- newElem "br"

  chatDiv <- newElem "div" `with`
    [
      attr "id" =: "chatDiv"
    ]


  chatsContainer <- newElem "div" `with`
    [
      attr "id"       =: "chats-container"
      --attr "rows"     =: "10",
      --attr "cols"     =: "18",
      --attr "readonly" =: "True"
    ]

  messageBox <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id"   =: "messageBox",
      attr "cols" =: "60"
    ]

  onEvent messageBox KeyPress $ \13 -> handleChatInput

  chatTabsHeader <- createChatTabsHeader

  appendChild parentDiv chatDiv
  appendChild chatDiv chatTabsHeader
  appendChild chatDiv chatsContainer
  appendChild chatDiv br
  appendChild chatDiv messageBox

  where
    handleChatInput :: Client ()
    handleChatInput = withElem "messageBox" $ \messageBox -> do
      message <- getValue messageBox
      setProp messageBox "value" ""
      case message of
        Just ""   -> return ()
        Nothing   -> return ()
        Just ('/':s) -> do
          let splitStr = words s
          let command = head splitStr
          let args = tail splitStr
          handleChatCommand command args
        Just s -> onServer $ sendChatMessage api <.> "main" <.> ChatMessage "" s

    handleChatCommand :: String -> [String] -> Client ()
    handleChatCommand c args
      | c == "join"  = let chatName = head args in do
        liftIO $ print $ "handleChatCommand > joining chat: " ++ chatName
        clientJoinChat api chatName
      | c == "msg"  =
        let chatName = head args
            chatMessage = unwords $ tail args
        in do
        liftIO $ print $ "handleCatCommand > msg chat: " ++ chatName ++ ": " ++ chatMessage
        onServer $ sendChatMessage api <.> chatName <.> ChatMessage "" chatMessage

    -- |Add the header container for chat tabs
    createChatTabsHeader :: Client Elem
    createChatTabsHeader = do
      chatTabsHeader <- newElem "ul" `with`
        [
          attr "id" =: "chat-tabs",
          attr "class" =: "nav nav-tabs",
          attr "role" =: "tablist"
        ]
      return chatTabsHeader

addNewChat :: String -> Client ()
addNewChat chatName = do
  addNewTabTotabsHeader chatName
  addNewChatToChatsContainer chatName

addNewTabTotabsHeader :: String -> Client ()
addNewTabTotabsHeader chatName =
  "chat-tabs" `withElem` \chatTabsHeader -> do
    chatTab <- newElem "li" `with`
      [
        attr "id"   =: ("chat-tab-" ++ chatName),
        attr "role" =: "presentation"
      ]
    textElem <- newTextElem chatName
    appendChild chatTab textElem
    appendChild chatTabsHeader chatTab

addNewChatToChatsContainer :: String -> Client ()
addNewChatToChatsContainer chatName =
  "chats-container" `withElem` \chatsContainer -> do
    chatContainer <- newElem "div" `with`
      [
        attr "id" =: ("chat-container-" ++ chatName),
        attr "class" =: "tab-pane"
      ]
    appendChild chatsContainer chatContainer

-- | Client joins the named chat and starts listen to it's messages
clientJoinChat :: LobbyAPI -> String -> Client ()
clientJoinChat api chatName = do
  onServer $ joinChat api <.> chatName
  addNewChat chatName
  fork $ listenForChatMessages api chatName $ chatMessageCallback chatName

-- | Called when a ChatMessage is received
chatMessageCallback :: String -> ChatMessage -> Client ()
chatMessageCallback chatName (ChatMessage from content) = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ ": " ++ content
  pushToChatBox $ from ++ ": " ++ content
chatMessageCallback chatName (ChatAnnounceJoin from)    = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ " has joined"
  pushToChatBox $ from ++ "has joined"
chatMessageCallback chatName (ChatAnnounceLeave from)   = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ " has left"
  pushToChatBox $ from ++ "has left"
chatMessageCallback chatName (ChatError errorMessage)   = do
  liftIO $ print $ "chatMessageCallback > " ++ "ChatError" ++ errorMessage
  pushToChatBox $ "ChatError" ++ errorMessage
chatMessageCallback chatName _ =
  liftIO $ print $ "chatMessageCallback > Bad ChatMessage on chat " ++ chatName

-- |Pushes the String arguments to the "chatBox" textarea
pushToChatBox :: String -> Client ()
pushToChatBox str = "chat-container-main" `withElem` \chatContainer -> do
  br <- newElem "br"
  appendChild chatContainer br
  textElem <- newTextElem str
  appendChild chatContainer textElem
  return ()

-- |Listen for chat messages on a chatChannel until the chat announces that the client leaves
-- |Start this method with fork $ listenForChatMessage args...
listenForChatMessages :: LobbyAPI -> String -> (ChatMessage -> Client ()) -> Client ()
listenForChatMessages api chatName callBack = do
  msg <- onServer $ readChatChannel api <.> chatName
  callBack msg
  -- Handle leave messages
  -- If this client that's leaving. then stop listening for new ChatMessages
  case msg of
    ChatAnnounceLeave from -> do
      clientName <- onServer $ getClientName api
      unless (from == clientName) $
        listenForChatMessages api chatName callBack
    _ -> listenForChatMessages api chatName callBack
