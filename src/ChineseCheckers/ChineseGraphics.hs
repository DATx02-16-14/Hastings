module ChineseCheckers.ChineseGraphics where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import ChineseCheckers.ChineseCheckers
import ChineseCheckers.Table
import ChineseCheckers.ChineseBitmaps
import qualified Control.Concurrent as CC
import qualified Haste.Concurrent as HC
import Haste.App
import LobbyAPI


radius :: Double
radius = 30

initTable2' can = mapM_ (renderSquare can 15 20)

drawSquare :: Double -> Double -> Square -> Picture ()
drawSquare space size (Square (Piece color) _ (x,y)) = do
    setFillColor color
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5)) size
drawSquare space size (Square Empty col (x,y)) = do
    setFillColor white
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5)) size

initTableCoords :: [Square] -> [((Int,Int),(Double,Double))]
initTableCoords = map (initTableCoord2 15 20)

initTableCoord :: Double -> Double -> Square -> ((Int,Int),(Double,Double))
initTableCoord space size (Square _ _ (x,y)) = ((x,y), (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5)))

initTableCoord2 :: Double -> Double -> Square -> ((Int,Int),(Double,Double))
initTableCoord2 space size (Square _ _ (x,y)) = ((x,y), (size/2 + size*fromIntegral x + space*fromIntegral (x+5), size/2 + size* fromIntegral y+space* fromIntegral (y+5)))


-- | Generate a canvas with the specified width and height in pixels
makeCanvas :: Int -> Int -> Client Elem
makeCanvas width height = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    set canvas
        [ prop "width"  =: show width
        , prop "height" =: show height
        ]
    return canvas

-- | Generate a button with the given text
mkButton :: String -> Client Elem
mkButton text = do
    button <- newElem "button"
    set button [prop "innerHTML" =: text]
    return button


drawGame :: CC.MVar GameState -> Elem -> LobbyAPI -> String -> Client HandlerInfo
-- | Inits the graphics
drawGame stateOfGame par api name = do
    gameState <- liftIO $ CC.takeMVar stateOfGame
    canvas <- makeCanvas 1400 800
    appendChild par canvas
    canvas2 <- makeCanvas 500 800
    appendChild par canvas2
    Just can <- liftIO $ fromElem canvas --  :: Client (Maybe Canvas)
    Just can2 <- liftIO $ fromElem canvas2 -- :: Client (Maybe Canvas)
    button <- mkButton "Rotate player"
    appendChild par button
    initTable2' can $ gameTable gameState
    liftIO $ CC.putMVar stateOfGame gameState
    onEvent can Click $ \mouse ->
      do
       state <- liftIO $ CC.takeMVar stateOfGame
       case currentPlayer state == currentPlayer state of  -- must save the clients name somehow
         True ->
          let (x,y) = mouseCoords mouse
             in
               case mapCoords (fromIntegral x,fromIntegral y) of
                 Nothing            ->
                  do
                   liftIO $ CC.putMVar stateOfGame state
                   return ()
                 Just (x1,y1)       ->
                            do
                             let newState = playerAction state (x1,y1)
                             case fromCoord newState of

                              Just (x,y) -> do
                               liftIO $ CC.putMVar stateOfGame newState
                               initTable2' can (gameTable newState)
                               renderSquare2 can 15 20 (squareContent (gameTable newState) (x,y) ) (x,y)
                               renderOnTop can2 $ text (50,50) "hejsan2"
                               case fromCoord newState of 
                                Nothing -> onServer $ writeGameChan api <.> Move (x1,y1) (x,y)
                                _ -> return ()
                               case playerDone (players newState) newState of
                                 Nothing -> graphicGameOver can
                                 Just x  -> liftIO $ CC.putMVar stateOfGame x

                              Nothing -> do
                               liftIO $ CC.putMVar stateOfGame newState
                               initTable2' can (gameTable $ playerAction state (x1,y1))
                               renderSquare2 can 15 20 (squareContent (gameTable newState) (x,y)) (x,y)
                             where colors = map snd
         False -> do
          liftIO $ CC.putMVar stateOfGame state
          return ()
    onEvent button Click $ \_ ->
     do
      liftIO $ do
       gameState <- liftIO $ CC.takeMVar stateOfGame
       let newState = rotatePlayer gameState
       initTable2' can (gameTable newState)
       render can2 $ scale (5,5) $ text (0,10) ( (currentPlayer (newState)) ++ "s speltur!!!" ++ ((showColor . snd . head) $ players newState))
       liftIO $ CC.putMVar stateOfGame $ rotatePlayer gameState
      onServer $ writeGameChan api <.> RotatePlayer 

-- | Render the game over text
graphicGameOver can = do
      bitmap <- loadBitmap "file:////home/benjamin/Documents/cooltext170130995424459.gif"
      renderOnTop can $ draw bitmap (10,10)

playerDone :: [(String,Color)] -> GameState -> Maybe GameState
playerDone [] t = Nothing
playerDone ((s,c):xs) state | playerHome c (gameTable state) = Just GameState {gameTable = gameTable state
                                             , currentPlayer = currentPlayer state
                                             , players = xs
                                             , fromCoord = fromCoord state
                                             , playerMoveAgain = playerMoveAgain state}
                            | otherwise = Just state

skrep :: GameState -> GameState
skrep gs = GameState {gameTable = startTable, currentPlayer = mao $ tail (players gs), players = tail (players gs) ++ [head (players gs)], fromCoord = fromCoord gs, playerMoveAgain = False}
   where mao [(x,y)] = x


mapCoords :: (Double,Double) -> Maybe (Int,Int)
mapCoords c1 = case mapCoords' c1 of
                [] -> Nothing
                _  -> Just . fst . head $ mapCoords' c1

mapCoords' :: (Double,Double) -> [((Int,Int),(Double,Double))]
mapCoords' c1 = filter wasDas $ initTableCoords startTable
        where wasDas (c2,c3) = distance c1 c3 <= radius

-- | Calculate the distance between two points
distance :: (Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2


showColor :: Color -> String
showColor color | color == red = "Red"
                | color == blue = "Blue"
                | color == yellow = "Yellow"
                | color == orange = "Orange"
                | color == green = "Green"
                | color == purple = "Purple"
