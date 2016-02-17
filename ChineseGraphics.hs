import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events

import Table hiding (Color)

data GameState = GameState { gameTable :: Table
                           , currentPlayer :: String
                           , players :: [(String,Color)]
                           , fromCoord :: Maybe Coord }

starOfDavid :: Double -> Double -> Shape ()
starOfDavid space size = do

    line (12.0*space+12.0*size, space) (space, 12.0*space+12.0*size)
    line (space, 12.0*space+12.0*size) (24.0*space+24.0*size, 12.0*space+12.0*size)
    line (24.0*space+24.0*size, 12.0*space+12.0*size) (12.0*space+12.0*size, space)
    line (space, 5.0*size+5.0*space) (24.0*space+24.0*size, 5.0*size+5.0*space)
    line (space, 5.0*size+5.0*space) (12.0*space+12.0*size, 16.0*size+16.0*space)
    line (12.0*space+12.0*size, 16.0*size+16.0*space) (24.0*space+24.0*size, 5.0*size+5.0*space)


--drawHoles :: Double -> Double -> Shape ()
--drawHoles space size = do
--        drawHoles 

--fill doesnt work?
starOfDavidInABox :: Picture ()
starOfDavidInABox = do
    stroke $ starOfDavid 20 20


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


--main :: IO ()
main = do
    canvas <- mkCanvas 1900 800
    appendChild documentBody canvas
    Just can <- fromElem canvas
    render can starOfDavidInABox
    onEvent can Click $ \mouse -> do
       let (x,y) = mouseCoords mouse
           (x1,y1) = mapCoords (x,y)
       render can starOfDavidInABox
    render can starOfDavidInABox

mapCoords = undefined