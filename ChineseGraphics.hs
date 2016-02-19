import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import ChineseCheckers
import qualified Control.Concurrent as CC
import Table

starOfDavid :: Double -> Double -> Shape ()
starOfDavid space size = do


      path [((12+5-1)*space+12*size, space*(5-2)), ((12+5+1)*space+12*size, space*(5-2)),
            ((16+5+1)*space+16*size, size*4+space*(4+5-2)), ((24+5+1)*space+24*size, size*4+space*(4+5-2)),
            ((24.5+1+5)*space+25*size, size*5+space*(5+5-2)), ((21+1+5)*space+21*size, size*9+space*(9+5-2)),
            ((24.5+1+5)*space+25*size, size*13+space*(13+5-2)), ((24+5+1)*space+24*size, size*14+space*(14+5-2)),
            ((16+5+1)*space+16*size, size*14+space*(14+5-2)), ((12+5+1)*space+12*size, size*18+space*(18+5-2)),
            ((12+5-1)*space+12*size, size*18+space*(18+5-2)), ((8+5-1)*space+8*size, size*13.5+space*(13.5+5-2)),
            ((8+5-1)*space+8*size, size*13.5+space*(13.5+5-2)), ((5-1)*space+0*size, size*13.5+space*(13.5+5-2)), 
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


initTable' :: Picture ()
initTable' = sequence_ $ map (drawSquare 15 20) startTable

starOfDavidInABox :: Picture ()
starOfDavidInABox = do
    fill $ starOfDavid 15 20
    initTable'


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

drawGame :: GameState -> Shape ()
drawGame = undefined

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
           (x1,y1) = mapCoords (x,y) in 
           do 
            gameState <- CC.takeMVar stateOfGame
            CC.putMVar stateOfGame $ skrep gameState
            render can2 $ text (50,50) ((currentPlayer gameState) ++ "s speltur!!!")

skrep :: GameState -> GameState
skrep gs = GameState {gameTable = startTable, currentPlayer = mao $ tail (players gs), players = (tail (players gs)) ++ [head (players gs)], fromCoord = fromCoord gs, playerMoveAgain = False}
   where mao [(x,y)] = x

mapCoords = undefined
