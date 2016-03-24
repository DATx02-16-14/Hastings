import ChineseCheckers
import ChineseGraphics
import Table
import Control.Concurrent
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events

parseGameAction :: GameAction -> GameState -> GameState
parseGameAction StartGame _ = undefined
parseGameAction RotatePlayer gs = rotatePlayer gs
parseGameAction (Move c1 c2) gs = playerAction newState c2

   where newState = undefined --mkState (Move c1 c2) gs

{-
mkState :: GameAction -> GameState -> GameState
mkState (Move c1 c2) state = GameState {gameTable = playerAction state 
                                             , currentPlayer = currentPlayer state
                                             , players = players state
                                             , fromCoord = Just c1
                                             , playerMoveAgain = playerMoveAgain state}
-}

runGame :: parent -> GameChan -> [String] -> IO HandlerInfo
runGame parent chan players = do
                                gameState <- newEmptyMVar
                                putMVar gameState $ initGame players
                                drawGame gameState chan documentBody
                               -- gameLoop chan gameState

--chan :: IO (GameChan)
chan = newChan

main = do 
    channel <- chan
    runGame documentBody channel ["pelle","lars","erich","Per"]


test channel = do
        writeChan channel StartGame
        hej <- readChan channel
        putStrLn $ show hej

test2 = do
         channel <- chan
         test channel

gameLoop :: GameChan -> MVar GameState -> IO ()
gameLoop chan state = do
                 action <- readChan chan
                 gs <- takeMVar state
                 putMVar state $ parseGameAction action gs
