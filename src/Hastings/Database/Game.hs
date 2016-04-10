module Hastings.Database.Game where

import Hastings.Database.Common (runDB)
import Hastings.Database.Fields

import qualified Database.Esqueleto as Esql

import Haste.App (SessionID)
import Data.Maybe (listToMaybe)

-- |Retrieve a game from the database.
retrieveGameByName :: String -- ^The name of the game to retrieve.
                   -> IO (Maybe (Esql.Entity Game))
retrieveGameByName name = runDB $ Esql.getBy $ UniqueName name

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

-- |Save a game to the database.
saveGame :: String    -- ^The UUID of the game to save.
         -> String    -- ^The name of the game to save.
         -> Int       -- ^Maximum amount of players for this game.
         -> SessionID -- ^The sessionID of the owner of the game.
         -> String    -- ^The password of the game, empty if the game has no password.
         -> IO (Esql.Key Game)
saveGame uuid name maxAmountOfPlayers owner password = runDB $ Esql.insert $ Game uuid name maxAmountOfPlayers owner password


-- |Add a player to a game
addPlayerToGame :: SessionID -- ^The sessionID of the player.
                -> Esql.Key Game -- ^The key of the game.
                -> IO (Esql.Key PlayerInGame)
addPlayerToGame sessionID gameKey = runDB $ Esql.insert $ PlayerInGame gameKey sessionID

-- |Retrieves a players that are currently in a game.
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