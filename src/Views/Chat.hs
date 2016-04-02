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
import Data.List (find)
import Data.Char (toLower)
import Control.Monad (unless, when)

import LobbyTypes
import LobbyAPI
import Views.Common

-- Html ID constants
chatTabId = "chat-tabs"
chatContainerId = "chat-container"
inputContainerId = "input-container"
chatTabIdPrefix = "chat-tab-"
inputFieldIdPrefix = "input-field-"
chatContainerIdPrefix = "chat-container-"

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
      attr "id"       =: chatContainerId
    ]

  inputContainer <- newElem "div" `with`
    [
      attr "id" =: inputContainerId
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
  (inputFieldIdPrefix ++ currentChatName) `withElem` \inputField -> do
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
      | c == "join" = if null args
        then
          pushToChatBox chatName "Missing chatname, use: /join [CHATNAME]"
        else let chatName' = head args in
          clientJoinChat api chatName'
      | c == "leave" =
        if null args
          then do
            clientLeaveChat api chatName chatName
            setActiveChat "main"
          else do
            let chatName' = head args
            clientLeaveChat api chatName chatName'
            when (chatName == chatName') $ setActiveChat "main"
            return ()
      | c == "msg" =
        let chatName    = head args
            chatMessage = unwords $ tail args in
        onServer $ sendChatMessage api <.> chatName <.> ChatMessage "" chatMessage

-- |Add the header container for chat tabs
createChatTabsHeader :: Client Elem
createChatTabsHeader =
  newElem "ul" `with`
    [
      attr "id" =: chatTabId,
      attr "class" =: "nav nav-tabs",
      attr "role" =: "tablist"
    ]

-- | Adds new DOM elements for a the named chat.
addNewChatDOM :: LobbyAPI -> String -> Client ()
addNewChatDOM api chatName = do
  addTabToTabHeader chatName
  addChatToChatContainer chatName
  addInputFieldToInputFieldsContainer api chatName

-- | Adds a new named tab to the tabs header.
-- | Clicks on this tabs changes changes to only show its respective chat window and input field
addTabToTabHeader :: String -> Client ()
addTabToTabHeader chatName =
  chatTabId `withElem` \chatTabsHeader -> do
    chatTab <- newElem "li" `with`
      [
        attr "id"   =: (chatTabIdPrefix ++ chatName),
        attr "role" =: "presentation"
      ]
    onEvent chatTab Click $ \_ -> setActiveChat chatName

    textElem <- newTextElem chatName
    linkWrapper <- newElem "a"
    appendChild linkWrapper textElem
    appendChild chatTab linkWrapper
    appendChild chatTabsHeader chatTab

-- | Adds a new chat window to the chat container.
-- | The given string is used to set an id attribute.
addChatToChatContainer :: String -> Client ()
addChatToChatContainer chatName =
  chatContainerId `withElem` \chatContainer -> do
    chatDiv <- newElem "div" `with`
      [
        attr "id" =: (chatContainerIdPrefix ++ chatName),
        attr "style" =: "overflow: auto; height: 200px; word-wrap: break-word;"
      ]
    appendChild chatContainer chatDiv

-- | Adds a new input field to the inputs container.
-- | The input field forwards the message to the handleChatInput function when 'enter' is pressed.
addInputFieldToInputFieldsContainer :: LobbyAPI -> String -> Client ()
addInputFieldToInputFieldsContainer api chatName =
  inputContainerId `withElem` \inputFieldContainer -> do
    inputField <- newElem "input" `with`
      [
        attr "id" =: (inputFieldIdPrefix ++ chatName),
        attr "type" =: "text",
        attr "cols" =: "60"

      ]
    onEvent inputField KeyPress $ \13 -> handleChatInput api chatName
    appendChild inputFieldContainer inputField
    return ()

-- | Removes the DOM elements corresponding to the named chat
deleteChatDOM :: String -> Client ()
deleteChatDOM chatName = do
  liftIO $ print $ chatTabIdPrefix       ++ chatName
  liftIO $ print $ chatContainerIdPrefix ++ chatName
  liftIO $ print $ inputFieldIdPrefix    ++ chatName
  deleteDOM (chatTabIdPrefix       ++ chatName) chatTabId
  deleteDOM (chatContainerIdPrefix ++ chatName) chatContainerId
  deleteDOM (inputFieldIdPrefix    ++ chatName) inputContainerId
  return ()

-- | Show the proper DOM elements for a named chat.
-- | All other chat elements are hidden.
setActiveChat :: String -> Client ()
setActiveChat chatName = do
  chatTabId `withElem` \chatTabs -> do
    setClassOnChildren chatTabs "active" False
    maybeChatTab <- elemById $ chatTabIdPrefix ++ chatName
    setClassOnMaybeDOMElem maybeChatTab "active" True

  chatContainerId `withElem` \chatContainer -> do
    setClassOnChildren chatContainer "hide" True
    maybeChatContainer <- elemById $ chatContainerIdPrefix ++ chatName
    setClassOnMaybeDOMElem maybeChatContainer "hide" False

  inputContainerId `withElem` \inputs -> do
    setClassOnChildren inputs "hide" True
    maybeInputField <- elemById $ inputFieldIdPrefix ++ chatName
    setClassOnMaybeDOMElem maybeInputField "hide" False

    maybe  (return ()) focus maybeInputField
  return ()
    where
      setClassOnChildren parent value doSet = do
        children <- getChildren parent
        mapM_ (\c -> setClass c value doSet) children

      setClassOnMaybeDOMElem maybeDOMElem value doSet =
        case maybeDOMElem of
          Nothing   -> return ()
          Just chat -> do
            setClass chat value doSet
            scrollToBottom chat

-- | Client joins the named chat and starts listen to it's messages
clientJoinChat :: LobbyAPI -> String -> Client ()
clientJoinChat api chatName = do
  joinedChats <- onServer $ getJoinedChats api
  if lowerCaseChatName `elem` (map stringToLower joinedChats)
    then
      setActiveChat chatName
    else do
      chats <- onServer $ getChats api
      let chatToJoin = fromMaybe chatName $ find ((lowerCaseChatName ==). stringToLower) chats
      onServer $ joinChat api <.> chatToJoin
      addNewChatDOM api chatToJoin
      fork $ listenForChatMessages api chatToJoin $ chatMessageCallback api chatToJoin
      setActiveChat chatToJoin
  return ()
  where
    lowerCaseChatName = stringToLower chatName
    stringToLower = map toLower

-- | Tells the server that the client wants to leave the named chat.
-- | Special case. You can't leave the chat named "main".
clientLeaveChat :: LobbyAPI -> String -> String -> Client ()
clientLeaveChat api activeChat chatName =
  if chatName == "main"
    then pushToChatBox activeChat "You can't leave the main chat"
    else do
      joinedChats <- onServer $ getJoinedChats api
      case find ((lowerCaseChatName ==). stringToLower) joinedChats of
        Nothing             -> do
          pushToChatBox activeChat $ "No chat named " ++ chatName
          return ()
        Just actualChatName -> do
          onServer $ leaveChat api <.> actualChatName
          deleteChatDOM actualChatName
  where
    lowerCaseChatName = stringToLower chatName
    stringToLower = map toLower


-- | Called when a ChatMessage is received
chatMessageCallback :: LobbyAPI -> String -> ChatMessage -> Client ()
chatMessageCallback api chatName (ChatMessage from content) =
  pushToChatBox chatName $ from ++ ": " ++ content
chatMessageCallback api chatName (ChatAnnounceJoin from)    =
  pushToChatBox chatName $ from ++ " has joined " ++ chatName
chatMessageCallback api chatName (ChatAnnounceLeave from)   = do
  pushToChatBox chatName $ from ++ " has left " ++ chatName
  --thisClientName <- onServer $ getClientName api
  --when (from == thisClientName) $ deleteChatDOM chatName
  --return ()
chatMessageCallback api chatName (ChatError errorMessage)   = do
  liftIO $ print $ "chatMessageCallback > " ++ "ChatError" ++ errorMessage
  pushToChatBox "main" $ "ChatError" ++ errorMessage
chatMessageCallback api chatName _ =
  liftIO $ print $ "chatMessageCallback > Bad ChatMessage on chat " ++ chatName

-- |Pushes the String arguments to the "chatBox" textarea
pushToChatBox :: String -> String -> Client ()
pushToChatBox chatName str = (chatContainerIdPrefix ++ chatName) `withElem` \chatContainer -> do
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
      joinedChats <- onServer $ getJoinedChats api
      when (chatName `elem` joinedChats) $
        listenForChatMessages api chatName callBack
    _ -> listenForChatMessages api chatName callBack
