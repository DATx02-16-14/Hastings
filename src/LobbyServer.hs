module LobbyServer(handshake, closeConnection, createGame, getGamesList, playerJoinGame, LobbyGame)
  where
import Haste.App
import qualified Control.Concurrent as CC
import Data.List
import Data.Maybe
import LobbyTypes

handshake :: Server PlayerList -> Name -> Server ()
handshake remotePlayers name = do
  players <- remotePlayers
  sid <- getSessionID
  liftIO $ CC.modifyMVar_ players  $ \ps ->
    return $ (sid,name) : ps

closeConnection :: Server PlayerList -> SessionID -> Server ()
closeConnection remotePlayers sid = do
  players <- remotePlayers
  liftIO $ CC.modifyMVar_ players $ \ps ->
    return $ filter ((sid /=) . fst) ps

createGame :: Server GamesList -> Server PlayerList -> Server (String,String)
createGame remoteGames remotePlayers = do
    players <- remotePlayers
    games <- remoteGames
    sid <- getSessionID
    playerList <- liftIO $ CC.readMVar players
    let maybePlayer = find (\p -> fst p == sid) playerList
    liftIO $ CC.modifyMVar_ games $ \gs ->
        case maybePlayer of
            Just p -> do
              game <- liftIO $ CC.newMVar ("string",[p])
              return $ game : gs
            Nothing -> return gs
    case maybePlayer of
        Just p -> return ("string", snd p)
        Nothing -> return ("false", "")

getGamesList :: Server GamesList -> Server [String]
getGamesList remoteGames = do
  gameList <- remoteGames >>= liftIO . CC.readMVar
  liftIO $ mapM (\g -> do
    game <- CC.readMVar g
    return $ fst game) gameList

playerJoinGame :: Server PlayerList -> Server GamesList -> String -> Server ()
playerJoinGame remotePlayers remoteGames gameID = do
  players <- remotePlayers >>= liftIO . CC.readMVar
  gameList <- remoteGames
  sid <- getSessionID
  case find (\(plrSid, _) -> plrSid == sid) players of
    Just plr -> liftIO $ CC.modifyMVar_ gameList $
      \gList -> addPlayerToGame plr gameID gList

    _ -> return ()

  return ()

-- Adds a player to a lobby game.
-- Is perhaps overly complicated since a LobbyGame is an MVar.
addPlayerToGame :: Player -> String -> [LobbyGame] -> IO [LobbyGame]
addPlayerToGame plr gameID gameList = do
  ga <- findIO (\game -> do
                 g <- CC.readMVar game
                 return $ (fst g) == gameID) gameList []
  case ga of
    (Just mVarGame, hs, ts) -> do
      modGame <- CC.modifyMVar mVarGame $
        \g -> do
          let (sessionID, gamePlayers) = g
          return $ ((sessionID, plr:gamePlayers), (sessionID, plr:gamePlayers))
      g <- CC.newMVar modGame
      return $ hs ++ (g:ts)
    (Nothing, _, _) -> error "addPlayerToGame: Could not add player"


    where
      -- Method that finds with IO and then returns the value
      -- together with the list to the right and left of the element
      findIO :: (a -> IO Bool) -> [a] -> [a] -> IO (Maybe a, [a], [a])
      findIO _ [] hs = return (Nothing, [], hs)
      findIO p (a:as) hs = do
        b <- p a
        case b of
          True  -> return $ (Just a, hs, as)
          False -> findIO p as (a:hs)