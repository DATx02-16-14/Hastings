module Server.Game where

import Haste.App (SessionID)
import Control.Concurrent (modifyMVar_, readMVar)
import Data.UUID
import System.Random
import Data.ByteString.Char8 (ByteString, empty, pack, unpack)
import Crypto.PasswordStore (makePassword, verifyPassword)
import Control.Monad (when)

import Hastings.Utils
import Hastings.ServerUtils
import LobbyTypes

-- |Removes a player from it's game
leaveGame :: GamesList -> SessionID -> IO ()
leaveGame mVarGames sid = do
  gamesList <- readMVar mVarGames
  case findGameWithSid sid gamesList of
    Nothing                 -> return ()
    Just game@(_, gameData) -> do
      modifyMVar_ mVarGames $ \games ->
        return $ updateListElem (removePlayer sid) (== game) games
      maybe
        (return ())
        (\p -> messageClients KickedFromGame [p])
        (lookupClientEntry sid $ players gameData)
      messageClients PlayerLeftGame $ players gameData
  where
    removePlayer sid (uuid, gameData) =
      let newClientList = filter ((sid /=) . sessionID) $ players gameData in
      (uuid, gameData {players = newClientList})

createGame :: GamesList -> ConcurrentClientList -> SessionID -> Int -> IO (Maybe String)
createGame mVarGames mVarClients sid maxPlayers = do
  clientList <- readMVar mVarClients
  gen <- newStdGen
  let (uuid, g) = random gen
  let uuidStr = Data.UUID.toString uuid

  maybe
    (return Nothing)
    (\c -> do
      modifyMVar_ mVarGames $ \gs ->
        return $ (uuidStr, GameData [c] "GameName" maxPlayers empty) : gs
      messageClients GameAdded clientList
      return $ Just uuidStr)
    (lookupClientEntry sid clientList)

-- |Lets a player join a game
playerJoinGame :: ConcurrentClientList  -- ^The list of all players connected
               -> GamesList             -- ^The list of all games
               -> SessionID             -- ^The SessionID of the player
               -> String                -- ^The UUID of the game to join
               -> String                -- ^The password of the game, if no password this can be ""
               -> IO Bool               -- ^Returns if able to join or not
playerJoinGame mVarClients mVarGames sid gameID passwordString = do
  clientList <- readMVar mVarClients
  gamesList <- readMVar mVarGames
  case (lookupClientEntry sid clientList, findGameWithID gameID gamesList) of
    (Just player, Just (_,gameData)) -> do
      let passwordOfGame = gamePassword gameData
      if passwordOfGame == empty || verifyPassword (pack passwordString) passwordOfGame
        then if maxAmountOfPlayers gameData > length (players gameData)
          then do
            modifyMVar_ mVarGames $
              \gList -> return $ addPlayerToGame player gameID gList
            messageClients PlayerJoinedGame (players gameData)
            return True
          else do
            messageClients (LobbyError "Game is full") [player]
            return False
        else do
          messageClients (LobbyError "Wrong password") [player]
          return False
    _                                -> return False

-- |Finds the name of a game given it's identifier
findGameNameWithID :: GamesList -> String -> IO String
findGameNameWithID mVarGames gameID = do
  gamesList <- readMVar mVarGames
  case findGameWithID gameID gamesList of
    Just (_, gameData) -> return $ gameName gameData
    Nothing            -> return ""

-- |Finds the name of the game the client is currently in
findGameNameWithSid :: GamesList -> SessionID -> IO String
findGameNameWithSid mVarGames sid = do
  gamesList <- readMVar mVarGames
  case findGameWithSid sid gamesList of
    Just (_, gameData) -> return $ gameName gameData
    Nothing            -> return ""

-- |Finds the name of the players of the game the current client is in
playerNamesInGameWithSid :: GamesList -> SessionID -> IO [String]
playerNamesInGameWithSid mVarGames sid = do
  gamesList <- readMVar mVarGames
  case findGameWithSid sid gamesList of
    Nothing            -> return []
    Just (_, gameData) -> return $ map name (players gameData)

-- |Kicks the player with index 'Int' from the list of players in
-- the game that the current client is in.
kickPlayerWithSid :: GamesList -> SessionID -> Int -> IO ()
kickPlayerWithSid mVarGames sid clientIndex = do
  gamesList <- readMVar mVarGames
  case findGameWithSid sid gamesList of
    Nothing   -> return ()
    Just game@(_,gameData) -> do
      modifyMVar_ mVarGames $ \games ->
        return $ updateListElem (deletePlayerFromGame clientIndex) (== game) games
      messageClients KickedFromGame [players gameData !! clientIndex]
      messageClients PlayerLeftGame $ players gameData

-- |Change the name of a 'LobbyGame' that the connected client is in
changeGameNameWithSid :: GamesList -> ConcurrentClientList-> SessionID -> Name -> IO ()
changeGameNameWithSid mVarGames mVarClients sid newName = do
  clientList <- readMVar mVarClients
  gamesList <- readMVar mVarGames
  case findGameWithSid sid gamesList of
    Nothing           -> return ()
    Just game@(_,gameData) -> do
      modifyMVar_ mVarGames $ \games ->
        return $ updateListElem
          (\(guuid, gData) -> (guuid, gData {gameName = newName}))
          (== game)
          games
      messageClients GameNameChange clientList

-- |Changes the maximum number of players for a game
-- Requires that the player is the last in the player list (i.e. the owner)
changeMaxNumberOfPlayers :: GamesList -> SessionID -> Int -> IO ()
changeMaxNumberOfPlayers mVarGames sid newMax = do
  gamesList <- readMVar mVarGames
  when (isOwnerOfGame sid gamesList) $
    case findGameWithSid sid gamesList of
      Nothing   -> return ()
      Just game ->
        modifyMVar_ mVarGames $ \games ->
          return $ updateListElem
            (\(guuid, gData) -> (guuid, gData {maxAmountOfPlayers = newMax}))
            (== game)
            games
-- |Sets the password (as a 'ByteString') of the game the client is in.
-- |Only possible if the client is the owner of the game.
setPasswordToGame :: GamesList -> SessionID -> String -> IO ()
setPasswordToGame mVarGames sid passwordString = do
  let password = pack passwordString
  hashedPassword <- makePassword password 17
  gamesList <- readMVar mVarGames
  case (findGameWithSid sid gamesList, isOwnerOfGame sid gamesList) of
    (Just game, True)          ->
      modifyMVar_ mVarGames $ \games ->
        return $ updateListElem
          (\(guuid, gData) -> (guuid, gData {gamePassword = hashedPassword}))
          (== game)
          games
    (Just (_,gameData), False) -> do
      maybe
        (return ())
        (\client -> messageClients (LobbyError "Not owner of the game") [client])
        (lookupClientEntry sid (players gameData))

-- |Returns True if game is password protected, False otherwise. 'String' is the UUID of the game
isGamePasswordProtected :: GamesList -> String -> IO Bool
isGamePasswordProtected mVarGames guuid = do
  gamesList <- readMVar mVarGames
  case findGameWithID guuid gamesList of
    Nothing           -> return False
    Just (_,gameData) -> return $ gamePassword gameData /= empty
