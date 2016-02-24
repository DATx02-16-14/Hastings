-- |All types that are used by the Lobby are placed in here.
module LobbyTypes where
import qualified Control.Concurrent as CC
import Haste.App

-- |A type synonym to clarify that some Strings are Names
type Name = String
-- |A player is a tuple with it's sessionID (specific to that connection), and it's name
type Player = (SessionID, Name)
-- |A list with all the players connected to the game
type PlayerList = CC.MVar [Player]
-- |A game inside of the lobby.
type LobbyGame = CC.MVar (String, [Player])
-- |A list of all the 'LobbyGame's that have been started inside the Lobby.
type GamesList = CC.MVar [LobbyGame]
-- |A list of all the chats in the lobby
type ChatList = CC.MVar [(Name, [Player])]

type LobbyState = (Server PlayerList, Server GamesList, Server ChatList)