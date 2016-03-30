-- |All types that are used by the Lobby are placed in here.
module LobbyTypes where
import qualified Control.Concurrent as CC
import Haste.App
import Data.List
import Data.Word
import Haste.Binary (Binary, Get)

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

-- |Data relevant to a game. Currently includes:
-- |The players who are in the game (where the last one is the owner)
-- |The name of the game
-- |The maximum allowed players, can be changed by the owner
data GameData = GameData {players            :: [ClientEntry],
                          gameName           :: Name,
                          maxAmountOfPlayers :: Int}
  deriving (Eq)

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
data LobbyMessage = NickChange | GameNameChange | KickedFromGame | GameAdded | ClientJoined | ClientLeft | PlayerJoinedGame
  deriving (Eq)

instance Binary LobbyMessage where
  put NickChange       = put (0 :: Word8)
  put GameNameChange   = put (1 :: Word8)
  put KickedFromGame   = put (2 :: Word8)
  put GameAdded        = put (3 :: Word8)
  put ClientJoined     = put (4 :: Word8)
  put ClientLeft       = put (5 :: Word8)
  put PlayerJoinedGame = put (6 :: Word8)

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> return NickChange
      1 -> return GameNameChange
      2 -> return KickedFromGame
      3 -> return GameAdded
      4 -> return ClientJoined
      5 -> return ClientLeft
      6 -> return PlayerJoinedGame

instance Binary Bool where
  put True  = put (0 :: Word8)
  put False = put (1 :: Word8)

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> return True
      1 -> return False
