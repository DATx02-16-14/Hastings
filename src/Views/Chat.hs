module Views.Chat
    (
    listenForChatMessages,
    chatMessageCallback,
    createChatDOM
    ) where

import Haste.App
import Haste
import Haste.Events
import Haste.DOM
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

  chatBox <- newElem "textarea" `with`
    [
      attr "id"       =: "chatBox",
      attr "rows"     =: "10",
      attr "cols"     =: "18",
      attr "readonly" =: "True"
    ]

  messageBox <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id"   =: "messageBox",
      attr "cols" =: "60"
    ]

  onEvent messageBox KeyPress $ \13 -> handleChatInput

  appendChild parentDiv chatDiv
  appendChild chatDiv chatBox
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
        liftIO $ print $ "handleCatCommand > joining chat: " ++ chatName
        onServer $ joinChat api <.> chatName
        listenForChatMessages api chatName chatMessageCallback
      | c == "msg"  =
        let chatName = head args
            chatMessage = unwords $ tail args
        in do
        liftIO $ print $ "handleCatCommand > msg chat: " ++ chatName ++ ": " ++ chatMessage
        onServer $ sendChatMessage api <.> chatName <.> ChatMessage "" chatMessage

-- | Called when a ChatMessage is received
chatMessageCallback :: ChatMessage -> Client ()
chatMessageCallback (ChatMessage from content) = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ ": " ++ content
  pushToChatBox $ from ++ ": " ++ content
chatMessageCallback (ChatAnnounceJoin from)    = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ " has joined"
  pushToChatBox $ from ++ "has joined"
chatMessageCallback (ChatAnnounceLeave from)   = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ " has left"
  pushToChatBox $ from ++ "has left"
chatMessageCallback _ =
  liftIO $ print "chatMessageCallback > Bad ChatMessage"

-- |Pushes the String arguments to the "chatBox" textarea
pushToChatBox :: String -> Client ()
pushToChatBox str = "chatBox" `withElem` \chatBox -> do
  prevValue <- getProp chatBox "value"
  setProp chatBox "value" $ prevValue ++ "\n" ++ str
  scrollHeight <- getProp chatBox "scrollHeight"
  setProp chatBox "scrollTop" scrollHeight
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
