module Hastings.Database.Game where

import Hastings.Database.Common (runDB)
import Hastings.Database.Fields

import qualified Database.Esqueleto as Esql
import qualified Database.Persist.Sql as Pers

import Haste.App (SessionID)
import Data.Maybe (listToMaybe)
import Control.Monad (liftM)
import Data.ByteString.Char8 (ByteString)

-- |Retrieve a game from the database.
retrieveGameByUUID :: String -- ^The UUID of the game
                   -> IO (Maybe (Esql.Entity Game))
retrieveGameByUUID uuid = runDB $ Esql.getBy $ UniqueUUID uuid

-- |Retrieves the game from the database that the player is currently in.
retrieveGameBySid :: SessionID -- ^The sessionID of the player.
                  -> IO (Maybe (Esql.Entity Game))
retrieveGameBySid sid = runDB $ do
  games <- Esql.select $
    Esql.from $ \(games, playersInGame) -> do
      Esql.where_ (games Esql.^. GameId Esql.==. playersInGame Esql.^. PlayerInGameGame
          Esql.&&. playersInGame Esql.^. PlayerInGamePlayer Esql.==. Esql.val sid)
      return games
  return $ listToMaybe games

-- |Retrieves all games from the database
retrieveAllGames :: IO [Esql.Entity Game]
retrieveAllGames = runDB $ Pers.selectList ([] :: [Pers.Filter Game]) []

-- |Save a game to the database.
saveGame :: String        -- ^The UUID of the game to save.
         -> String        -- ^The name of the game to save.
         -> Int           -- ^Maximum amount of players for this game.
         -> SessionID     -- ^The sessionID of the owner of the game.
         -> ByteString    -- ^The password of the game, empty if the game has no password.
         -> IO (Esql.Key Game)
saveGame uuid name maxAmountOfPlayers owner password = runDB $ Esql.insert $ Game uuid name maxAmountOfPlayers owner password

-- |Set password on game
setPasswordOnGame :: String     -- ^The UUID of the game
                  -> ByteString -- ^The password of the game
                  -> IO ()
setPasswordOnGame uuid password = runDB $
  Esql.update $ \games -> do
    Esql.set games [GamePassword Esql.=. Esql.val password]
    Esql.where_ (games Esql.^. GameUuid Esql.==. Esql.val uuid)

-- |Set the name of the game
setNameOnGame :: String -- ^The UUID of the game
              -> String -- ^The new name of the game
              -> IO ()
setNameOnGame uuid name = runDB $
  Esql.update $ \games -> do
    Esql.set games [GameName Esql.=. Esql.val name]
    Esql.where_ (games Esql.^. GameUuid Esql.==. Esql.val uuid)

-- |Set the number of players in a game
setNumberOfPlayersInGame :: String -- ^The UUID of the game.
                         -> Int    -- ^The new number of players.
                         -> IO ()
setNumberOfPlayersInGame uuid numberOfPlayers = runDB $
  Esql.update $ \games -> do
      Esql.set games [GameMaxAmountOfPlayers Esql.=. Esql.val numberOfPlayers]
      Esql.where_ (games Esql.^. GameUuid Esql.==. Esql.val uuid)

-- |Set the owner of a game.
setGameOwner :: Esql.Key Game -- ^The key of the game.
             -> SessionID     -- ^The sessionID of the new owner.
             -> IO ()
setGameOwner gameKey sid = runDB $
  Esql.update $ \games -> do
    Esql.set games [GameOwner Esql.=. Esql.val sid]
    Esql.where_ (games Esql.^. GameId Esql.==. Esql.val gameKey)

-- |Add a player to a game
addPlayerToGame :: SessionID     -- ^The sessionID of the player.
                -> Esql.Key Game -- ^The key of the game.
                -> IO (Esql.Key PlayerInGame)
addPlayerToGame sessionID gameKey = runDB $ Esql.insert $ PlayerInGame gameKey sessionID

-- |Remove a player from a game
removePlayerFromGame :: SessionID     -- ^The sessionID od the player.
                     -> Esql.Key Game -- ^The key of the game.
                     -> IO ()
removePlayerFromGame sessionID gameKey = runDB $
  Esql.delete $ Esql.from $ \playersInGame ->
    Esql.where_ (playersInGame Esql.^. PlayerInGameGame Esql.==. Esql.val gameKey
        Esql.&&. playersInGame Esql.^. PlayerInGamePlayer Esql.==. Esql.val sessionID)

-- |Retrieves all players that are currently in a specific game.
retrievePlayersInGame :: Esql.Key Game -- ^The key of the game.
                      -> IO [Esql.Entity Player]
retrievePlayersInGame gameKey = runDB $
      Esql.select $ Esql.from $ \players -> do
        (playerInGame, onlinePlayers) <- onlinePlayerInGame
        Esql.where_ (onlinePlayers Esql.^. OnlinePlayerPlayer Esql.==. players Esql.^. PlayerId
            Esql.&&. playerInGame Esql.^. PlayerInGameGame Esql.==. Esql.val gameKey)
        return players
  where
    onlinePlayerInGame = Esql.from $ \(playerInGame `Esql.InnerJoin` onlinePlayers) -> do
        Esql.on (playerInGame Esql.^. PlayerInGamePlayer Esql.==. onlinePlayers Esql.^. OnlinePlayerSessionID)
        return (playerInGame, onlinePlayers)

-- |Retrieves the sessionID of all players in a game.
retrieveSessionIdsInGame :: Esql.Key Game -- ^The key of the game.
                         -> IO [SessionID]
retrieveSessionIdsInGame gameKey = runDB $ do
  players <- Esql.select $ Esql.from $ \playersInGame -> do
    Esql.where_ (playersInGame Esql.^. PlayerInGameGame Esql.==. Esql.val gameKey)
    return playersInGame
  return $ map (playerInGamePlayer . Esql.entityVal) players

-- |Retrieves the number of players currently in a game.
retrieveNumberOfPlayersInGame :: String -- ^The UUID of the game
                              -> IO Int
retrieveNumberOfPlayersInGame uuid = do
  g <- retrieveGameByUUID uuid
  case g of
    Just game -> liftM length $ retrievePlayersInGame (Esql.entityKey game)
    Nothing   -> return 0
