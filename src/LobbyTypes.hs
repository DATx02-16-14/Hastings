-- |All types that are used by the Lobby are placed in here.
module LobbyTypes where
import qualified Control.Concurrent as CC
import Haste.App

-- |A type synonym to clarify that some Strings are Names.
type Name = String
-- |A player is name and it's chat channel.
data Player = Player {name :: Name
                     ,chatChannel :: CC.Chan ChatMessage}
-- |A client entry is a player with a SessionID as key.
type ClientEntry = (SessionID, Player)
-- |A list with all the players connected to the game.
type ClientMap = CC.MVar [ClientEntry]
-- |A game inside of the lobby.

type LobbyGame = CC.MVar (String, [Player])
-- |A list of all the 'LobbyGame's that have been started inside the Lobby.
type GamesList = CC.MVar [LobbyGame]
-- | The state of the lobby being passed around.

type LobbyState = (Server ClientMap, Server GamesList, Server ConcurrentChatList)

-- |A chat message sent on a channel.
data ChatMessage = ChatMessage {from    :: SessionID
                               ,content :: String}
-- |A chat has a name and all sessionIDs currently in the chat.
type Chat = (Name,[SessionID])
-- |A list of all the chats in the lobby.
type ConcurrentChatList = CC.MVar [Chat]
