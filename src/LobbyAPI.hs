{-# LANGUAGE CPP #-}
-- |A module for defining the api the server provides towards the client
module LobbyAPI where
import Haste.App
import qualified Control.Concurrent as CC
import LobbyTypes
#ifdef __HASTE__
#define REMOTE(x) (remote undefined)
#else
import qualified Server
#define REMOTE(x) (remote x)
#endif
-- |The api provided by the server.
data LobbyAPI = LobbyAPI
  { connect                   :: Remote (String -> Server ())
    -- |Creates a game on the server with the current client as host.
    -- |The 'Int' represents the default max number of players
  , createGame                :: Remote (Int -> Server (Maybe String))
  , getGamesList              :: Remote (Server [String])
    -- |Joins a game with the 'UUID' representetd by the 'String'.
    -- |The second 'String' is the password for the game, can be left as "" if there is no password.
    -- |Returns if the client successfully joined or not.
  , joinGame                  :: Remote (String -> String -> Server Bool)
  , findPlayersInGame         :: Remote (Server [String])
    -- |Finds the name of the game with String as identifier
  , findGameNameWithID        :: Remote (String -> Server String)
    -- |Finds the name of the game that the client is in
  , findGameName              :: Remote (Server String)
  , getPlayerNameList         :: Remote (Server [String])
    -- |Kicks a player frrom a game.
  , kickPlayer                :: Remote (Int -> Server ())
    -- |Changes the nickname of the active player
  , changeNickName            :: Remote (Name -> Server ())
    -- |Change the name of the game to the new name
  , changeGameName            :: Remote (Name -> Server())
    -- |Reads the value from the lobby channel
  , readLobbyChannel          :: Remote (Server LobbyMessage)
   -- |Changes the maximum amount of players
   , changeMaxNumberOfPlayers :: Remote (Int -> Server ())
    -- |Join named chat
  , joinChat                  :: Remote (Name -> Server ())
    -- |Leave named chat
  , leaveChat                 :: Remote (Name -> Server ())
    -- |Send ChatMessage over Named channel
  , sendChatMessage           :: Remote (Name -> ChatMessage -> Server ())
    -- |Reads next ChatMessage from named chat channel.
  , readChatChannel           :: Remote (Name -> Server ChatMessage)
    -- |Get list of chats the client is in
  , getJoinedChats            :: Remote (Server [String])
    -- |Get list of all chat names
  , getChats                  :: Remote (Server [String])
    -- |Sets a password to the game the client is in as 'ByteString'
    -- |Only allowed if the current player is owner of it's game
  , setPassword               :: Remote (String -> Server ())
    -- |Returns if the game is protected by a password or not. 'String' is the Game ID
  , isGamePasswordProtected   :: Remote (String -> Server Bool)
    -- |Returns whether or not the current player is owner of the game it's in
  , isOwnerOfCurrentGame      :: Remote (Server Bool)
    -- |Leaves the game the player is in
  , leaveGame                 :: Remote (Server ())
  }

-- |Creates an instance of the api used by the client to communicate with the server.
newLobbyAPI :: LobbyState -> App LobbyAPI
newLobbyAPI (playersList, gamesList, chatList) =
   LobbyAPI <$> REMOTE((Server.connect                  playersList))
            <*> REMOTE((Server.createGame               playersList))
            <*> REMOTE((Server.getGamesList             gamesList))
            <*> REMOTE((Server.playerJoinGame           playersList))
            <*> REMOTE((Server.playerNamesInGameWithSid ))
            <*> REMOTE((Server.findGameNameWithID       ))
            <*> REMOTE((Server.findGameNameWithSid      ))
            <*> REMOTE((Server.getConnectedPlayerNames  playersList))
            <*> REMOTE((Server.kickPlayerWithSid        gamesList))
            <*> REMOTE((Server.changeNickName           playersList gamesList))
            <*> REMOTE((Server.changeGameNameWithSid    playersList))
            <*> REMOTE((Server.readLobbyChannel         playersList))
            <*> REMOTE((Server.changeMaxNumberOfPlayers ))
            <*> REMOTE((Server.joinChat                 playersList chatList))
            <*> REMOTE((Server.leaveChat                playersList))
            <*> REMOTE((Server.sendChatMessage          playersList chatList))
            <*> REMOTE((Server.readChatChannel          playersList))
            <*> REMOTE((Server.getJoinedChats           playersList))
            <*> REMOTE((Server.getChats                 chatList))
            <*> REMOTE((Server.setPasswordToGame        gamesList))
            <*> REMOTE((Server.isGamePasswordProtected  gamesList))
            <*> REMOTE((Server.remoteIsOwnerOfGame      gamesList))
            <*> REMOTE((Server.leaveGame                playersList))
