module ChineseGraphics where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import ChineseCheckers
import Table
import qualified Control.Concurrent as CC
import qualified Haste.Concurrent as HC
import qualified Data.Map.Strict as Map


renderTest can = do
      bitmap <- loadBitmap "file:////home/michael/Documents/cooltext170130995424459.gif"
      renderOnTop can $ draw bitmap (10,10)

renderSquare2 can space size (Piece col) (x,y) 
        |col == blue = do
                bitmap <- loadBitmap "file:////home/michael/Documents/blue2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == green = do
                bitmap <- loadBitmap "file:////home/michael/Documents/green3.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == orange = do
                bitmap <- loadBitmap "file:////home/michael/Documents/orange2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == yellow = do
                bitmap <- loadBitmap "file:////home/michael/Documents/yellow2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == purple = do
                bitmap <- loadBitmap "file:////home/michael/Documents/purple2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == red = do
                bitmap <- loadBitmap "file:////home/michael/Documents/red2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)

filepath :: String
filepath = "file:////home/benjamin/Documents/"

starOfDavid' :: Double -> Double -> Shape ()
starOfDavid' space size = 

        path [((12+5-1)*space+12*size, space*(5-2)), ((12+5+1)*space+12*size, space*(5-2)),
              ((16+5+1)*space+16*size, size*4+space*(4+5-2)), ((24+5+1)*space+24*size, size*4+space*(4+5-2)),
              ((24.5+1+5)*space+25*size, size*5+space*(5+5-2)), ((21+1+5)*space+21*size, size*9+space*(9+5-2)),
              ((24.5+1+5)*space+25*size, size*13+space*(13+5-2)), ((24+5+1)*space+24*size, size*14+space*(14+5-2)),
              ((16+5+1)*space+16*size, size*14+space*(14+5-2)), ((12+5+1)*space+12*size, size*18+space*(18+5-2)),
              ((12+5-1)*space+12*size, size*18+space*(18+5-2)), ((8+5-1)*space+8*size, size*14+space*(14+5-2)),
              ((8+5-1)*space+8*size, size*14+space*(14+5-2)), ((5-1)*space+0*size, size*14+space*(14+5-2)),
              ((3.5-1)*space+0*size, size*13+space*(13+5-2)), ((3+5-1)*space+3*size, size*9+space*(9+5-2)),
              ((3.5-1)*space+0*size, size*5+space*(5+5-2)), ((5-1)*space+0*size, size*4+space*(4+5-2)),
              ((8+5-1)*space+8*size, size*4+space*(4+5-2)), ((12+5-1)*space+12*size, space*(5-2))]

initTable2' can = mapM_ (renderSquare can 15 20)

--renderTable can  = do
--        bitmap <- loadBitmap "file:////home/benjamin/Documents/0305509001456402835_chinese_checkers_start_posit.png"
--        renderOnTop can $ scale (1.4,0.90) $ draw bitmap (20,20)
renderTable can  = do
        bitmap <- loadBitmap $ filepath ++ "0305509001456402835_chinese_checkers_start_posit.png"
        renderOnTop can $ scale (1.5,0.90) $ draw bitmap (0,20)


renderSquare can space size (Square Empty _ (x,y)) = do
        bitmap <- loadBitmap "file:////home/michael/Documents/empty.bmp"
        renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
renderSquare can space size (Square (Piece col) _ (x,y))
        |col == blue = do
                bitmap <- loadBitmap "file:////home/michael/Documents/blue.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == green = do
                bitmap <- loadBitmap "file:////home/michael/Documents/green.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == orange = do
                bitmap <- loadBitmap "file:////home/michael/Documents/orange.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == yellow = do 
                bitmap <- loadBitmap "file:////home/michael/Documents/yellow.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == purple = do
                bitmap <- loadBitmap "file:////home/michael/Documents/purple.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)
        |col == red = do
                bitmap <- loadBitmap "file:////home/michael/Documents/red.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+5)) (size* fromIntegral y+space* fromIntegral (y+5)) 40.0 40.0)


drawSquare :: Double -> Double -> Square -> Picture ()
drawSquare space size (Square (Piece color) _ (x,y)) = do
    setFillColor color
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5)) size
drawSquare space size (Square Empty col (x,y)) = do
    setFillColor white
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5)) size


radius :: Double
radius = 30

initTableCoords :: [Square] -> [((Int,Int),(Double,Double))]
initTableCoords = map (initTableCoord2 15 20)

initTableCoord :: Double -> Double -> Square -> ((Int,Int),(Double,Double))
initTableCoord space size (Square _ _ (x,y)) = ((x,y), (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5))) 

initTableCoord2 :: Double -> Double -> Square -> ((Int,Int),(Double,Double))
initTableCoord2 space size (Square _ _ (x,y)) = ((x,y), (size/2 + size*fromIntegral x + space*fromIntegral (x+5), size/2 + size* fromIntegral y+space* fromIntegral (y+5))) 

initTable' :: Table -> Picture ()
initTable' t = do
         fill $ starOfDavid' 15 20
         mapM_ (drawSquare 15 20) t

starOfDavid :: Picture ()
starOfDavid = do
    fill $ starOfDavid' 15 20
    initTable' startTable


makeCanvas :: Int -> Int -> IO Elem
makeCanvas width height = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    set canvas
        [ prop "width"  =: show width
        , prop "height" =: show height
        ]
    return canvas

mkButton :: String -> IO Elem
mkButton text = do
    button <- newElem "button"
    set button [prop "innerHTML" =: text]
    return button


--drawGame :: CC.MVar GameState -> CC.Chan (GameState) -> parent -> IO HandlerInfo
drawGame stateOfGame outbox par = do
    gameState <- CC.takeMVar stateOfGame
    canvas <- makeCanvas 1400 800
    appendChild par canvas
    canvas2 <- makeCanvas 500 800
    appendChild par canvas2
    Just can <- fromElem canvas :: IO (Maybe Canvas)
    Just can2 <- fromElem canvas2 :: IO (Maybe Canvas)
    button <- mkButton "Rotate player"
    appendChild par button
    initTable2' can $ gameTable gameState
    CC.putMVar stateOfGame gameState
    onEvent can Click $ \mouse -> do
      state <- CC.readMVar stateOfGame
      case (currentPlayer state) == "pelle" of  -- must save the clients name somehow
        True -> 
         let (x,y) = mouseCoords mouse
            in
              case mapCoords (fromIntegral x,fromIntegral y) of 
                Nothing            -> return ()
                Just (x1,y1)       -> 
                           do
                            gameState <- CC.takeMVar stateOfGame
                            let newState = playerAction gameState (x1,y1)
                            case fromCoord newState of

                             Just (x,y) -> do
                              CC.putMVar stateOfGame newState
                              CC.putMVar outbox $ Move (x1,y1) (x,y)
                              initTable2' can (gameTable $ newState)
  --                            renderSquare2 can 15 20 (squareContent (gameTable newState) (x,y)) (x,y)
                              renderOnTop can2 $ text (50,50) $ "hejsan2"
                              case playerDone (players newState) newState of
                                Nothing -> graphicGameOver can
                                Just x  -> CC.putMVar stateOfGame $ x

                             Nothing -> do
                              CC.putMVar stateOfGame newState
                              initTable2' can (gameTable $ playerAction gameState (x1,y1))
  --                            render can2 $ text (50,50) ((currentPlayer $ playerAction gameState (x1,y1)) ++ "s speltur!!!" ++ ((showColor . snd . head) $ players newState))
                              renderSquare2 can 15 20 (squareContent (gameTable newState) (x,y)) (x,y)
                            where colors xs = map snd xs
        False -> return ()

    onEvent button Click $ \_ -> 
     do
      gameState <- CC.takeMVar stateOfGame
      let newState = rotatePlayer gameState
      render can2 $ scale (5,5) $ text (0,10) ( (currentPlayer (newState)) ++ "s speltur!!!" ++ ((showColor . snd . head) $ players newState))
--      render can2 $ text (50,50) ( (currentPlayer (newState)) ++ "s speltur!!!" ++ ((showColor . snd . head) $ players newState))
      CC.putMVar stateOfGame $ rotatePlayer gameState
--      render can2 $ text (150,150) (currentPlayer $ rotatePlayer gameState)


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

distance :: (Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2


showColor :: Color -> String
showColor color | color == red = "Red"
                | color == blue = "Blue"
                | color == yellow = "Yellow"
                | color == orange = "Orange"
                | color == green = "Green"
                | color == purple = "Purple"
