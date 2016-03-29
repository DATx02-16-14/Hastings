import ChineseCheckers
import ChineseGraphics
import Table
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import qualified Haste.Concurrent as HC
import Control.Concurrent as CC
import Control.Concurrent.STM.TChan

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

runGame :: parent -> CC.MVar GameState -> CC.MVar GameAction -> [String] -> IO HandlerInfo
runGame parent gameState outbox players = do
                                CC.putMVar gameState $ initGame players
                                drawGame gameState outbox documentBody
--                                gameLoop chan gameState


--chan :: IO (GameChan)
chan = newChan

-- | The inbox, outbox and list of names will be supplied by the server
main = do
    state <- CC.newEmptyMVar
    inbox <- CC.newEmptyMVar
    outbox <- CC.newEmptyMVar
    forkIO $ updateState inbox state
    runGame documentBody state outbox ["pelle","lars","erich","Per"]


gameLoop :: GameChan -> MVar GameState -> IO ()
gameLoop chan state = do
                 action <- readChan chan
                 gs <- takeMVar state
                 putMVar state $ parseGameAction action gs


-- | Loop to update the current GameState in use of the client, meant to be forked
updateState :: CC.MVar GameAction -> CC.MVar GameState -> IO ()
updateState inbox stateOfGame = do
                                 move <- CC.takeMVar inbox
                                 state <- CC.takeMVar stateOfGame
                                 CC.putMVar stateOfGame $ parseGameAction move state
                                 updateState inbox stateOfGame



{-
updateState :: HC.Inbox GameAction -> HC.MVar GameState -> HC.CIO ()
updateState inbox stateOfGame = do
                                 move <- HC.receive inbox
                                 state <- HC.takeMVar stateOfGame
                                 HC.putMVar stateOfGame $ parseGameAction move state
                                 updateState inbox stateOfGame
-}