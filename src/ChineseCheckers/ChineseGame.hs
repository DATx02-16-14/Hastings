module ChineseCheckers.ChineseGame where

import ChineseCheckers.ChineseCheckers
import ChineseCheckers.ChineseGraphics
import ChineseCheckers.Table
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import qualified Haste.Concurrent as HC
import qualified Control.Concurrent as CC
import LobbyAPI
import Haste.App
import Haste.App.Concurrent

parseGameAction :: GameAction -> GameState -> GameState
parseGameAction RotatePlayer gs = rotatePlayer gs
parseGameAction m@(Move c1 c2) gs = mkState m gs

mkState :: GameAction -> GameState -> GameState
mkState (Move c1 c2) state = GameState {gameTable = movePiece (gameTable state) c1 c2
                                             , currentPlayer = currentPlayer $ rotatePlayer state
                                             , players = players state
                                             , fromCoord = Nothing
                                             , playerMoveAgain = playerMoveAgain state}
mkState _ _ = error "Unable to create GameState from GameAction"


runGame :: Elem -> CC.MVar GameState -> [String] -> String -> LobbyAPI -> Client ()
runGame parent gameState players name api = do
                                drawGame gameState documentBody api name
                                HC.fork $ listenForGameAction api gameState
                                liftIO $ do
                                    CC.putMVar gameState $ initGame players
                                 -- add function for communication handling
--                                gameLoop chan gameState

listenForGameAction :: LobbyAPI -> CC.MVar GameState -> Client ()
listenForGameAction api state = do
                        ga <- onServer $ readGameChan api
                        name <- onServer $ getClientName api
                        liftIO $ do
                          gs <- CC.takeMVar state
                          CC.putMVar state (parseGameAction ga gs)

                        listenForGameAction api state
        where   function :: LobbyAPI -> Remote (Server GameAction)
                function = undefined -- todo



-- | The inbox, outbox and list of names will be supplied by the server

{-
gameLoop :: GameChan -> CC.MVar GameState -> IO ()
gameLoop chan state = do
                 action <- readChan chan
                 gs <- takeMVar state
                 putMVar state $ parseGameAction action gs

-}
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
