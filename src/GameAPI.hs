-- |Contains the API a game has to implement
module GameAPI where
import Haste.DOM
import Haste.App

-- |The api a game has to implement to be able to be played from the lobby.
data GameAPI = GameAPI
  {
    -- |Gets the name of the game type. E.g. "Chineese checkers"
    getGameTypeName :: String
    -- |Getter for the max amount of players in a game
  , getMaxNumberOfPlayers :: Int
    -- |Getter for the minimum amount of players in a game
  , getMinNumberOfPlayers :: Int
    -- |Starts a game with a list of the name of the players and a parent div where the game can be drawn.
  , startGame :: [String] -> Elem -> Client ()
  }

newGameAPI :: GameAPI
newGameAPI = GameAPI "testGame" 6 2 undefined


