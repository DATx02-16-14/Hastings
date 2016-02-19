module ChineseGraphics
  where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import ChineseCheckers
import qualified Control.Concurrent as CC
import Table
import qualified Data.Map.Strict as Map

starOfDavid :: Double -> Double -> Shape ()
starOfDavid space size = do

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


drawSquare :: Double -> Double -> Square -> Picture ()
drawSquare space size (Square (Piece color) col (x,y)) = do
    setFillColor color
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5)) size
drawSquare space size (Square Empty col (x,y)) = do
    setFillColor white
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5)) size


radius :: Double
radius = 20

initTableCoords :: [Square] -> [((Int,Int),(Double,Double))]
initTableCoords s = map (initTableCoord 15 20) s

initTableCoord :: Double -> Double -> Square -> ((Int,Int),(Double,Double))
initTableCoord space size (Square _ _ (x,y)) = ((x,y), (size*fromIntegral x + space*fromIntegral (x+5),size* fromIntegral y+space* fromIntegral (y+5))) 

initTable' :: Table -> Picture ()
initTable' t = sequence_ $ map (drawSquare 15 20) t

starOfDavidInABox :: Picture ()
starOfDavidInABox = do
    fill $ starOfDavid 15 20
    initTable' startTable


mkCanvas :: Int -> Int -> IO Elem
mkCanvas width height = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    set canvas
        [ prop "width"  =: show width
        , prop "height" =: show height
        ]
    return canvas

--main :: IO ()
main = do
    stateOfGame <- CC.newEmptyMVar
    CC.putMVar stateOfGame $ initGame ["Pelle", "Lasse"]
    canvas <- mkCanvas 1900 800
    appendChild documentBody canvas
    canvas2 <- mkCanvas 500 500
    appendChild documentBody canvas2
    Just can <- fromElem canvas
    Just can2 <- fromElem canvas2
    render can starOfDavidInABox
    onEvent can Click $ \mouse ->
       let (x,y) = mouseCoords mouse
           (x1,y1) = mapCoords' (fromIntegral x,fromIntegral y)
          in 
           do
            gameState <- CC.takeMVar stateOfGame
            CC.putMVar stateOfGame $ playerAction gameState (x1,y1)
            render can $ initTable' (gameTable gameState)
            render can2 $ text (50,50) ("(" ++(show x1) ++ "," ++ (show y1)++ ")")
--            render can2 $ text (150,150) ("s speltur!!!")


skrep :: GameState -> GameState
skrep gs = GameState {gameTable = startTable, currentPlayer = mao $ tail (players gs), players = (tail (players gs)) ++ [head (players gs)], fromCoord = fromCoord gs, playerMoveAgain = False}
   where mao [(x,y)] = x




mapCoords' :: (Double,Double) -> (Int,Int)
mapCoords' c1 = fst . head . filter wasDas $ initTableCoords startTable
        where wasDas (c2,c3) = distance c1 c3 <= radius

distance :: (Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
