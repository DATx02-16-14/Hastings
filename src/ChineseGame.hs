module ChineseGame where

import ChineseCheckers
import ChineseGraphics
import Table
import Control.Concurrent
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events

data GameAction = StartGame | Move Coord Coord | RotatePlayer
    deriving (Show)

type GameChan = Chan (GameAction)

parseGameAction :: GameAction -> GameState -> GameState
parseGameAction StartGame _ = undefined
parseGameAction RotatePlayer gs = rotatePlayer gs
parseGameAction (Move c1 c2) gs = playerAction newState c2

    where newState = mkState (Move c1 c2) gs


mkState :: GameAction -> GameState -> GameState
mkState (Move c1 c2) state = GameState {gameTable = gameTable state
                                             , currentPlayer = currentPlayer state
                                             , players = players state
                                             , fromCoord = Just c1
                                             , playerMoveAgain = playerMoveAgain state}

runGame :: parent -> GameChan -> [String] -> IO ()
runGame parent chan players = do
                                gameState <- newEmptyMVar
                                putMVar gameState $ initGame players
                                printGame parent chan
                                gameLoop chan gameState


printGame = undefined


gameLoop :: GameChan -> MVar GameState -> IO ()
gameLoop chan state = do
                 action <- readChan chan
                 gs <- takeMVar state
                 putMVar state $ parseGameAction action gs
                 
