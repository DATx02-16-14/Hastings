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
parseGameAction m@(Coord (x,y)) gs = mkState m gs

mkState :: GameAction -> GameState -> GameState
mkState (Move c1 c2) state = GameState {gameTable = movePiece (gameTable state) c1 c2
                                             , currentPlayer = currentPlayer $ rotatePlayer state
                                             , players = players state
                                             , fromCoord = Nothing
                                             , playerMoveAgain = playerMoveAgain state}
mkState (Coord (x,y)) state = playerAction state (x,y)

mkState _ _ = error "Unable to create GameState from GameAction"


runGame :: Elem -> CC.MVar GameState -> [String] -> String -> LobbyAPI -> Client HandlerInfo
runGame parent gameState players name api = do
                                createPlayerTurnList parent players
                                setPlayerTurn name

                                liftIO . CC.putMVar gameState $ initGame players
                                canvas <- liftIO $ makeCanvas 1030 750 "gameCanvas"
                                appendChild parent canvas

                                Just can <- liftIO $ fromElem canvas --  :: Client (Maybe Canvas)
                                button <- liftIO $ mkButton "Rotate player"
                                appendChild parent button
                                HC.fork $ listenForGameAction api gameState can
                                 -- add function for communication handling
--                                gameLoop chan gameState
                                drawGame gameState can button api name

listenForGameAction :: LobbyAPI -> CC.MVar GameState -> Canvas -> Client ()
listenForGameAction api state can = do
                        ga <- onServer $ readGameChan api
                        name <- onServer $ getClientName api
                        gs <- liftIO $ CC.takeMVar state
                        let newState = parseGameAction ga gs
                        setPlayerTurn $ currentPlayer newState
                        case fromCoord newState of
                               Just (x,y) -> do
                                  initTable2' can $ gameTable newState
                                  renderSquare2 can widthPiece heightPiece (squareContent (gameTable newState) (x,y)) (x,y)
                               Nothing -> initTable2' can $ gameTable newState
                        liftIO $ CC.putMVar state newState

                        listenForGameAction api state can
        where   function :: LobbyAPI -> Remote (Server GameAction)
                function = undefined -- todo


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
