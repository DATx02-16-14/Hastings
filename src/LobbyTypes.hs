-- |All types that are used by the Lobby are placed in here.
module LobbyTypes where
import qualified Control.Concurrent as CC
import Haste.App
import Data.List

-- |A type synonym to clarify that some Strings are Names.
type Name = String
-- |A client entry is a player with a SessionID as key.
data ClientEntry = ClientEntry {sessionID    :: SessionID
                               ,name         :: Name
                               ,chatChannel  :: CC.Chan ChatMessage
                               ,lobbyChannel :: CC.Chan LobbyMessage}
instance Eq ClientEntry where
  c1 == c2 = sessionID c1 == sessionID c2
  c1 /= c2 = sessionID c1 == sessionID c2

-- |A list with all the players connected to the game.
type ConcurrentClientList = CC.MVar [ClientEntry]
-- |A game inside of the lobby.
type LobbyGame = (String, GameData)

data GameData = GameData {players     :: [ClientEntry],
                          gameName    :: Name}

-- |A list of all the 'LobbyGame's that have been started inside the Lobby.
type GamesList = CC.MVar [LobbyGame]
-- | The state of the lobby being passed around.

type LobbyState = (Server ConcurrentClientList, Server GamesList, Server ConcurrentChatList)

-- |A chat message sent on a channel.
data ChatMessage = ChatMessage {from    :: SessionID
                               ,content :: String}
-- |A chat has a name and all sessionIDs currently in the chat.
type Chat = (Name,[SessionID])
-- |A list of all the chats in the lobby.
type ConcurrentChatList = CC.MVar [Chat]

lookupClientEntry :: SessionID -> [ClientEntry] -> Maybe ClientEntry
lookupClientEntry sid = find ((sid ==) . sessionID)

-- |LobbyMessage is a message to a client idicating some udate to the state that the cliet has to adapt to.
data LobbyMessage = NickChange String String
