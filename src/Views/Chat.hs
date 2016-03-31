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
import Data.Maybe
import Control.Monad (unless)

import LobbyTypes
import LobbyAPI
import Views.Common

-- | Create chat DOM in the left column
createChatDOM :: LobbyAPI -> Client ()
createChatDOM api = do
  leftContent <- elemById "leftContent"
  createChatDOMInParent api $ fromJust leftContent

-- | Create chat DOM and append it to the specified parent element.
createChatDOMInParent :: LobbyAPI -> Elem -> Client ()
createChatDOMInParent api parentDiv = do

  br <- newElem "br"

  chatDiv <- newElem "div" `with`
    [
      attr "id" =: "chatDiv"
    ]


  chatContainer <- newElem "div" `with`
    [
      attr "id"       =: "chat-container"
      --attr "rows"     =: "10",
      --attr "cols"     =: "18",
      --attr "readonly" =: "True"
    ]

  inputContainer <- newElem "div" `with`
    [
      attr "id" =: "input-container"
    ]

  chatTabsHeader <- createChatTabsHeader

  appendChild parentDiv chatDiv
  appendChild chatDiv chatTabsHeader
  appendChild chatDiv chatContainer
  appendChild chatDiv br
  appendChild chatDiv inputContainer

-- | Given a message, forward it to server if it's an ordinary chat message.
-- | Also handle commands. commands begin with a "/"
handleChatInput :: LobbyAPI -> String -> Client ()
handleChatInput api currentChatName =
  ("input-field-" ++ currentChatName) `withElem` \inputField -> do
    message <- getValue inputField
    setProp inputField "value" ""
    case message of
      Just ""   -> return ()
      Nothing   -> return ()
      Just ('/':s) -> do
        let splitStr = words s
        let command = head splitStr
        let args = tail splitStr
        handleChatCommand command args currentChatName
      Just s -> onServer $ sendChatMessage api <.> currentChatName <.> ChatMessage "" s
  where
    handleChatCommand :: String -> [String] -> String -> Client ()
    handleChatCommand c args chatName
      | c == "join"  = let chatName' = head args in do
        liftIO $ print $ "handleChatCommand > joining chat: " ++ chatName'
        clientJoinChat api chatName'
      | c == "leave"  = do
        liftIO $ print $ "handleChatCommand > leaving chat: " ++ chatName
        if null args
          then
            clientLeaveChat api chatName
          else let chatName' = head args in
            clientLeaveChat api chatName'
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

addNewChat :: LobbyAPI -> String -> Client ()
addNewChat api chatName = do
  addTabToTabHeader chatName
  addChatToChatContainer chatName
  addInputFieldToInputFieldsContainer api chatName

-- | Adds a new named tab to the tabs header.
-- | Clicks on this tabs changes changes to only show its respective chat window and input field
addTabToTabHeader :: String -> Client ()
addTabToTabHeader chatName =
  "chat-tabs" `withElem` \chatTabsHeader -> do
    chatTab <- newElem "li" `with`
      [
        attr "id"   =: ("chat-tab-" ++ chatName),
        attr "role" =: "presentation"
      ]
    onEvent chatTab Click $ \_ -> setActiveChat chatName

    textElem <- newTextElem chatName
    appendChild chatTab textElem
    appendChild chatTabsHeader chatTab

-- | Adds a new chat window to the chat container.
-- | The given string is used to set an id attribute.
addChatToChatContainer :: String -> Client ()
addChatToChatContainer chatName =
  "chat-container" `withElem` \chatContainer -> do
    chatDiv <- newElem "div" `with`
      [
        attr "id" =: ("chat-container-" ++ chatName),
        attr "style" =: "overflow: auto; height: 200px; word-wrap: break-word;"
      ]
    appendChild chatContainer chatDiv

-- | Adds a new input field to the inputs container.
-- | The input field forwards the message to the handleChatInput function when 'enter' is pressed.
addInputFieldToInputFieldsContainer :: LobbyAPI -> String -> Client ()
addInputFieldToInputFieldsContainer api chatName =
  "input-container" `withElem` \inputFieldContainer -> do
    inputField <- newElem "input" `with`
      [
        attr "id" =: ("input-field-" ++ chatName),
        attr "type" =: "text",
        attr "cols" =: "60"

      ]
    onEvent inputField KeyPress $ \13 -> handleChatInput api chatName
    appendChild inputFieldContainer inputField
    return ()

deleteChat :: String -> Client ()
deleteChat chatName = do
  liftIO $ print $ "chat-tab-" ++ chatName
  liftIO $ print $ ("chat-container-" ++ chatName)
  liftIO $ print $ ("input-field-"    ++ chatName)
  deleteDOM ("chat-tab-"       ++ chatName) "chat-tabs"
  deleteDOM ("chat-container-" ++ chatName) "chat-container"
  deleteDOM ("input-field-"    ++ chatName) "input-container"
  return ()

-- | Show the proper DOM elements for a named chat.
-- | All other chat elements are hidden.
setActiveChat :: String -> Client ()
setActiveChat chatName = do
  "chat-container" `withElem` \chatContainer -> do
    setHideClassOnChildren chatContainer
    maybeDOMElem <- elemById $ "chat-container-" ++ chatName
    clearClassAttrOnMaybeDomElem maybeDOMElem
  "input-container" `withElem` \inputs -> do
    setHideClassOnChildren inputs
    maybeDOMElem <- elemById $ "input-field-" ++ chatName
    clearClassAttrOnMaybeDomElem maybeDOMElem
  return ()
    where
      setHideClassOnChildren parent = do
        children <- getChildren parent
        mapM_ (\c -> setAttr c "class" "hide") children

      clearClassAttrOnMaybeDomElem maybeDOMElem = do
        case maybeDOMElem of
          Nothing   -> return ()
          Just chat -> do
            setAttr chat "class" ""
            scrollToBottom chat

-- | Client joins the named chat and starts listen to it's messages
clientJoinChat :: LobbyAPI -> String -> Client ()
clientJoinChat api chatName = do
  maybeChatContainer <- elemById $ "chat-container-" ++ chatName
  case maybeChatContainer of
    Nothing -> do
      onServer $ joinChat api <.> chatName
      addNewChat api chatName
      fork $ listenForChatMessages api chatName $ chatMessageCallback chatName
      setActiveChat chatName
    Just _ ->
      setActiveChat chatName
  return ()

clientLeaveChat :: LobbyAPI -> String -> Client ()
clientLeaveChat api chatName = do
  onServer $ leaveChat api <.> chatName


-- | Called when a ChatMessage is received
chatMessageCallback :: String -> ChatMessage -> Client ()
chatMessageCallback chatName (ChatMessage from content) = do
  liftIO $ print $ "chatMessageCallback > Chat:{" ++ chatName ++ "} from:{" ++ from ++ "] message:{" ++ content ++ "}"
  pushToChatBox chatName $ from ++ ": " ++ content
chatMessageCallback chatName (ChatAnnounceJoin from)    = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ " has joined"
  pushToChatBox chatName $ from ++ " has joined " ++ chatName
chatMessageCallback chatName (ChatAnnounceLeave from)   = do
  liftIO $ print $ "chatMessageCallback > " ++ from ++ " has left"
  pushToChatBox chatName $ from ++ " has left " ++ chatName
  deleteChat chatName
chatMessageCallback chatName (ChatError errorMessage)   = do
  liftIO $ print $ "chatMessageCallback > " ++ "ChatError" ++ errorMessage
  pushToChatBox chatName $ "ChatError" ++ errorMessage
chatMessageCallback chatName _ =
  liftIO $ print $ "chatMessageCallback > Bad ChatMessage on chat " ++ chatName

-- |Pushes the String arguments to the "chatBox" textarea
pushToChatBox :: String -> String -> Client ()
pushToChatBox chatName str = ("chat-container-" ++ chatName) `withElem` \chatContainer -> do
  br <- newElem "br"
  appendChild chatContainer br
  textElem <- newTextElem str
  appendChild chatContainer textElem
  scrollToBottom chatContainer
  return ()

-- | Set set the scrollTop property equal to the scrollHeight property.
scrollToBottom :: Elem -> Client()
scrollToBottom scrollableElem = do
  scrollHeight <- getProp scrollableElem "scrollHeight"
  setProp scrollableElem "scrollTop" scrollHeight

-- |Listen for chat messages on a chatChannel until the chat announces that the client leaves
-- |Start this method with fork $ listenForChatMessage args...
listenForChatMessages :: LobbyAPI -> String -> (ChatMessage -> Client ()) -> Client ()
listenForChatMessages api chatName callBack = do
  msg <- onServer $ readChatChannel api <.> chatName
  callBack msg
  -- Handle leave messages
  -- If its this client that's leaving. then stop listening for new ChatMessages
  case msg of
    ChatAnnounceLeave from -> do
      clientName <- onServer $ getClientName api
      unless (from == clientName) $
        listenForChatMessages api chatName callBack
    _ -> listenForChatMessages api chatName callBack
