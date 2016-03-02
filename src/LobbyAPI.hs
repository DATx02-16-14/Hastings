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
  , createGame :: Remote (Server (Maybe (String,String)))
  , getGamesList :: Remote (Server [String])
  , joinGame :: Remote (String -> Server ())
  , findPlayersInGame :: Remote (String -> Server [String])
  , getPlayerNameList :: Remote(Server [String])
    -- |Kicks a player frrom a game.
  , kickPlayer :: Remote (String -> Name -> Server ())
    -- |Changes the nickname of the active player
  , changeNickName :: Remote (Name -> Server ())
  }

-- |Creates an instance of the api used by the client to communicate with the server.
newLobbyAPI :: LobbyState -> App LobbyAPI
newLobbyAPI (playersList, gamesList, chatList) =
   LobbyAPI <$> REMOTE((Server.connect playersList chatList))
            <*> REMOTE((Server.createGame gamesList playersList))
            <*> REMOTE((Server.getGamesList gamesList))
            <*> REMOTE((Server.playerJoinGame playersList gamesList))
            <*> REMOTE((Server.playerNamesInGame gamesList))
            <*> REMOTE((Server.getConnectedPlayerNames playersList))
            <*> REMOTE((Server.kickPlayer gamesList))
            <*> REMOTE((Server.changeNickName playersList gamesList))
