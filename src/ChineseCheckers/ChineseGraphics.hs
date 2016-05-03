module ChineseCheckers.ChineseGraphics where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import ChineseCheckers.ChineseCheckers
import ChineseCheckers.Table
--import ChineseCheckers.ChineseBitmaps
import qualified Control.Concurrent as CC
import qualified Haste.Concurrent as HC
import Haste.App
import LobbyAPI
import Haste.Foreign
import Haste.Prim

import ChineseCheckers.ChineseBitmaps (filepath)


-- | Constants used when drawing the game, used for scaling purposes
xShift = 5
yShift = 5
width = 30.0
height = 30.0

widthPiece = 10
heightPiece = 20

renderTest can = do
      bitmap <- loadBitmap $ filepath ++ "/cooltext170130995424459.gif"
      renderOnTop can $ draw bitmap (10,10)


renderSquare2 can space size (Piece col) (x,y)
        |col == blue = do
                bitmap <- loadBitmap $ filepath ++ "/blue2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == green = do
                bitmap <- loadBitmap $ filepath ++ "/green3.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == orange = do
                bitmap <- loadBitmap $ filepath ++ "/orange2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == yellow = do
                bitmap <- loadBitmap $ filepath ++ "/yellow2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == purple = do
                bitmap <- loadBitmap $ filepath ++ "/purple2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == red = do
                bitmap <- loadBitmap $ filepath ++ "/red2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)

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

initTable2' can = mapM_ (renderSquare can widthPiece heightPiece)

--renderTable can  = do
--        bitmap <- loadBitmap "file:////home/benjamin/Documents/0305509001456402835_chinese_checkers_start_posit.png"
--        renderOnTop can $ scale (1.4,0.90) $ draw bitmap (20,20)
renderTable can  = do
        bitmap <- loadBitmap $ filepath ++ "0305509001456402835_chinese_checkers_start_posit.png"
        renderOnTop can $ scale (1.5,0.90) $ draw bitmap (0,20)


renderSquare can space size (Square Empty _ (x,y)) = do
        bitmap <- loadBitmap $ filepath ++ "/empty.bmp"
        renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
renderSquare can space size (Square (Piece col) _ (x,y))
        |col == blue = do
                bitmap <- loadBitmap $ filepath ++ "/blue.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == green = do
                bitmap <- loadBitmap $ filepath ++ "/green.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == orange = do
                bitmap <- loadBitmap $ filepath ++ "/orange.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == yellow = do
                bitmap <- loadBitmap $ filepath ++ "/yellow.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == purple = do
                bitmap <- loadBitmap $ filepath ++ "/purple.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)
        |col == red = do
                bitmap <- loadBitmap $ filepath ++ "/red.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) width height)


drawSquare :: Double -> Double -> Square -> Picture ()
drawSquare space size (Square (Piece color) _ (x,y)) = do
    setFillColor color
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+xShift),size* fromIntegral y+space* fromIntegral (y+yShift)) size
drawSquare space size (Square Empty col (x,y)) = do
    setFillColor white
    fill $ circle (size*fromIntegral x + space*fromIntegral (x+xShift),size* fromIntegral y+space* fromIntegral (y+yShift)) size


radius :: Double
radius = 30

initTableCoords :: [Square] -> [((Int,Int),(Double,Double))]
initTableCoords = map (initTableCoord2 widthPiece heightPiece)

initTableCoord :: Double -> Double -> Square -> ((Int,Int),(Double,Double))
initTableCoord space size (Square _ _ (x,y)) = ((x,y), (size*fromIntegral x + space*fromIntegral (x+xShift),size* fromIntegral y+space* fromIntegral (y+yShift)))

initTableCoord2 :: Double -> Double -> Square -> ((Int,Int),(Double,Double))
initTableCoord2 space size (Square _ _ (x,y)) = ((x,y), (size/2 + size*fromIntegral x + space*fromIntegral (x+xShift), size/2 + size* fromIntegral y+space* fromIntegral (y+yShift)))

initTable' :: Table -> Picture ()
initTable' t = do
         fill $ starOfDavid' widthPiece heightPiece
         mapM_ (drawSquare widthPiece heightPiece) t

starOfDavid :: Picture ()
starOfDavid = do
    fill $ starOfDavid' widthPiece heightPiece
    initTable' startTable


makeCanvas :: Int -> Int -> String -> IO Elem
makeCanvas width height iden = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    set canvas
        [ prop "width"  =: show width
        , prop "height" =: show height
        , prop "id" =: iden
        ]
    return canvas

mkButton :: String -> IO Elem
mkButton text = do
    button <- newElem "button"
    set button [prop "innerHTML" =: text]
    return button

    --canId <- getAttr can "id"
getAbsoluteCords :: Canvas -> IO JSString
getAbsoluteCords = ffi $ toJSStr "(function(canvas) {\
            \var rect = canvas.getBoundingClientRect();\
            \return (rect.left + ':' + rect.top)\
          \})"


-- | Converts a JSString on the form x:y to a touple (Int,Int)
intTupleFromString :: String -> (Int, Int)
intTupleFromString str = (read hInt :: Int, read tInt :: Int)
  where
    hInt = takeWhile (/='.') h
    tInt = takeWhile (/='.') t
    (h, _:t) = break (==':') str


drawGame :: CC.MVar GameState -> Canvas -> Elem -> LobbyAPI -> String -> Client HandlerInfo
-- | Inits the graphics
drawGame stateOfGame can button api name = do
    gameState <- liftIO $ CC.takeMVar stateOfGame

    initTable2' can $ gameTable gameState
    liftIO $ CC.putMVar stateOfGame gameState
    onEvent can Click $ \(MouseData (x,y) _ _) -> -- \mouse ->
      do
       state <- liftIO $ CC.takeMVar stateOfGame
       if currentPlayer state == name  -- must save the clients name somehow
         then do
          absCordsCanvas <- liftIO $ getAbsoluteCords can
          let (canvasX, canvasY) = intTupleFromString $ fromJSStr absCordsCanvas
              --(x,y) = mouseCoords mouse
            in do
               liftIO . print $ "CanvasCoords: " ++ (show canvasX) ++ " " ++ (show canvasY)
               liftIO . print $ "MouseCoords : " ++ (show x) ++ " " ++ (show y)
               case mapCoords (fromIntegral $ x - canvasX, fromIntegral y) of
                 Nothing            ->
                  do
--                   liftIO $ print $ "(" ++ show x ++ "," ++ show y ++ ")"
                   liftIO $ CC.putMVar stateOfGame state
                   return ()
                 Just (x1,y1)       ->
                    do
                     liftIO $ print $ "(" ++ show x1 ++ "," ++ show y1 ++ ")"
                     let newState = playerAction state (x1,y1)
                     onServer $ writeGameChan api <.> Coord (x1,y1)
                     case fromCoord newState of

                      Just (x,y) -> do
                       liftIO $ print $ "(" ++ show x ++ "," ++ show y ++ ")"
                       liftIO $ CC.putMVar stateOfGame newState
                       initTable2' can (gameTable newState)
                       renderSquare2 can widthPiece heightPiece (squareContent (gameTable newState) (x,y) ) (x,y)
                       case playerDone (players newState) newState of
                         Nothing -> graphicGameOver can
                         Just x  -> liftIO $ CC.putMVar stateOfGame x

                      Nothing -> do
                       liftIO $ CC.putMVar stateOfGame newState
                       initTable2' can (gameTable $ playerAction state (x1,y1))
                       renderSquare2 can widthPiece heightPiece (squareContent (gameTable newState) (x,y)) (x,y)
                     where colors = map snd
         else do
          liftIO $ CC.putMVar stateOfGame state
          return ()
    onEvent button Click $ \_ -> do
      state <- liftIO $ CC.takeMVar stateOfGame
      if currentPlayer state == name
        then do
               liftIO $ CC.putMVar stateOfGame state
               onServer $ writeGameChan api <.> RotatePlayer
        else liftIO $ CC.putMVar stateOfGame state


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

createPlayerTurnList :: IsElem parent => parent -> [String] -> Client ()
createPlayerTurnList parent players = do
  playerTurnContainer <- newElem "div" `with` [
      attr "id" =: "player-turn-container"
    ]

  playerTurnList <- newElem "ul" `with` [
      attr "id"    =: "player-turn-list",
      attr "class" =: "nav nav-pills"
    ]

  mapM_ (\p -> do
      playerElem <- newElem "li" `with` [
          attr "id" =: ("player-turn-list-" ++ p)
        ]
      textElem <- newTextElem p
      linkWrapper <- wrapInParent textElem "a"
      appendChild playerElem linkWrapper
      appendChild playerTurnList playerElem
    ) players

  appendChild playerTurnContainer  playerTurnList
  appendChild parent playerTurnContainer
  return ()
    where
      --wrapInParent :: (IsElem e, MonadIO m) => e -> String -> m e
      wrapInParent child elemType = do
        parent <- newElem elemType
        appendChild parent child
        return parent


setPlayerTurn :: String -> Client ()
setPlayerTurn playerName = do
  playerTurnList <- elemById "player-turn-list"
  maybe (return ()) (\l ->
      getChildren l >>= mapM_ (\e -> setClass e "active" False)
    ) playerTurnList
  playerElem <- elemById ("player-turn-list-" ++ playerName)
  maybe (return ()) (\e -> setClass e "active" True) playerElem
  return ()
