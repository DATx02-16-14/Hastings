{-# LANGUAGE CPP #-}
-- |A module for defining the api the server provides towards the client
module LobbyAPI where
import Haste.App
import qualified Control.Concurrent as CC
import LobbyTypes
#ifdef __HASTE__
#define REMOTE(x) (remote undefined)
#else
import LobbyServer as Server
#define REMOTE(x) (remote x)
#endif
-- |The api provided by the server.
data LobbyAPI = LobbyAPI
  { connect :: Remote (String -> Server ())
  , createGame :: Remote (Server (Maybe (String)))
  , getGamesList :: Remote (Server [String])
  , joinGame :: Remote (String -> Server ())
  , findPlayersInGame :: Remote (String -> Server [String])
    -- |Finds the name of the game with String as identifier
  , findGameName :: Remote (String -> Server String)
  , getPlayerNameList :: Remote (Server [String])
    -- |Kicks a player frrom a game.
  , kickPlayer :: Remote (String -> Name -> Server ())
    -- |Changes the nickname of the active player
  , changeNickName :: Remote (Name -> Server ())
    -- |Change the name of the game with the String to the new name
  , changeGameName :: Remote (String -> Name -> Server())
    -- |Reads the value from the lobby channel
  , readLobbyChannel :: Remote (Server LobbyMessage)
  }

-- |Creates an instance of the api used by the client to communicate with the server.
newLobbyAPI :: LobbyState -> App LobbyAPI
newLobbyAPI (playersList, gamesList, chatList) =
   LobbyAPI <$> REMOTE((Server.connect playersList chatList))
            <*> REMOTE((Server.createGame gamesList playersList))
            <*> REMOTE((Server.getGamesList gamesList))
            <*> REMOTE((Server.playerJoinGame playersList gamesList))
            <*> REMOTE((Server.playerNamesInGameWithID gamesList))
            <*> REMOTE((Server.findGameNameWithID gamesList))
            <*> REMOTE((Server.getConnectedPlayerNames playersList))
            <*> REMOTE((Server.kickPlayerWithGameID gamesList))
            <*> REMOTE((Server.changeNickName playersList gamesList))
            <*> REMOTE((Server.changeGameNameWithID gamesList))
            <*> REMOTE((Server.readLobbyChannel playersList))
