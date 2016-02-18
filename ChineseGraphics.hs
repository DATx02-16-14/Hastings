import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import ChineseCheckers
import qualified Control.Concurrent as CC
import Table


-- | Colors used for checkers
red = RGB 255 0 0
green = RGB 0 255 0
blue = RGB 0 0 255
black = RGB 0 0 0
yellow = RGB 255 255 0
purple = RGB 153 0 153

starOfDavid :: Double -> Double -> Shape ()
starOfDavid space size = do

    line (12.0*space+12.0*size, space) (space, 12.0*space+12.0*size)
    line (space, 12.0*space+12.0*size) (24.0*space+24.0*size, 12.0*space+12.0*size)
    line (24.0*space+24.0*size, 12.0*space+12.0*size) (12.0*space+12.0*size, space)
    line (space, 5.0*size+5.0*space) (24.0*space+24.0*size, 5.0*size+5.0*space)
    line (space, 5.0*size+5.0*space) (12.0*space+12.0*size, 16.0*size+16.0*space)
    line (12.0*space+12.0*size, 16.0*size+16.0*space) (24.0*space+24.0*size, 5.0*size+5.0*space)



drawSquare :: Double -> Double -> Square -> Shape ()
drawSquare size space (Square cont col (x,y)) = do
    circle (size*fromIntegral x + space*fromIntegral (x+2),size* fromIntegral y+space* fromIntegral (y+2)) size

--initTable :: Picture ()
--initTable = sequence_ $ map (fill . setFillColor drawSquare 20 20) startTable


starOfDavidInABox :: Picture ()
starOfDavidInABox = do
    stroke $ starOfDavid 20 20
--    initTable

drawHolesInABox :: Picture ()
drawHolesInABox = undefined


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

drawGame = undefined

--main :: IO ()
main = do
    stateOfGame <- CC.newEmptyMVar
    CC.putMVar stateOfGame initGame
    canvas <- mkCanvas 1900 800
    appendChild documentBody canvas
    Just can <- fromElem canvas
    render can starOfDavidInABox
    onEvent can Click $ \mouse ->
       let (x,y) = mouseCoords mouse
           (x1,y1) = mapCoords (x,y) in 
           do 
            gameState <- CC.takeMVar stateOfGame
            CC.putMVar stateOfGame $ playerAction gameState (x1,y1)
            render can drawGame
    render can starOfDavidInABox

-- | For test purposes only!!!
initGame :: GameState
initGame = GameState {gameTable = startTable
                     , currentPlayer = "Pelle"
                     , players = []
                     , fromCoord = Nothing
                     , playerMoveAgain = False}


mapCoords = undefined