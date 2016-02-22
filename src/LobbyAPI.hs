{-# LANGUAGE CPP #-}
module LobbyAPI where
import Haste.App
import qualified Control.Concurrent as CC
import LobbyTypes
#ifdef __HASTE__
#define REMOTE(x) (remote undefined)
#else
import qualified LobbyServer as Server
#define REMOTE(x) (remote x)
#endif

data LobbyAPI = LobbyAPI
  { handshake :: Remote (String -> Server ())
  , createGame :: Remote (Server (String,String))
  , getGamesList :: Remote (Server [String])
  , joinGame ::Remote (String -> Server ())
  , findPlayersInGame :: Remote (String -> Server [String])
  , getPlayerList :: Remote(Server [String])
  }

newLobbyAPI :: Server PlayerList -> Server GamesList -> App LobbyAPI
newLobbyAPI playersList gamesList = do
   LobbyAPI <$> REMOTE((Server.handshake playersList))
            <*> REMOTE((Server.createGame gamesList playersList))
            <*> REMOTE((Server.getGamesList gamesList))
            <*> REMOTE((Server.playerJoinGame playersList gamesList))
            <*> REMOTE((Server.playerNamesInGame gamesList))
            <*> REMOTE((Server.getConnectedPlayers playersList))
