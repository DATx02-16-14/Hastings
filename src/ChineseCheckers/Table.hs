module ChineseCheckers.Table where
import Haste.Graphics.Canvas
import Control.Concurrent
import Haste.Binary
import Data.Word
import Haste
import Haste.DOM
import Haste.Events

type Table = [Square]
--data Color = blue | red | purple | green | orange | yellow | white

data Content = Empty | Piece Color
    deriving (Show, Eq)

data Square = Square Content Color Coord
    deriving (Show, Eq)

data GameAction = StartGame | Move Coord Coord | RotatePlayer | GameActionError String | Coord (Int,Int)



type Player = String
type GameChan = Chan GameAction



data GameState = GameState { gameTable :: Table
                           , currentPlayer :: String
                           , players :: [(String,Color)]
                           , fromCoord :: Maybe Coord
                           , playerMoveAgain :: Bool }

type Coord = (Int,Int)

instance Eq Color where
    (==) (RGB c1 c2 c3) (RGB c4 c5 c6) = c1 == c4 && c2 == c5 && c3 == c6

instance Show Color where
    show (RGB a b c) = "RGB " ++ show a ++ " " ++ show b ++ " " ++ show c

instance Binary GameAction where
    put StartGame = put (0 :: Word8)
    put RotatePlayer = put (1 :: Word8)
    put (Coord (x1,y1)) = do
            put (3 :: Word8)
            put (fromIntegral x1 :: Word8)
            put (fromIntegral y1 :: Word8)
    put (Move (x1,y1) (x2,y2)) = do
                         put (2 :: Word8)
                         put (fromIntegral x1 :: Word8)
                         put (fromIntegral y1 :: Word8)
                         put (fromIntegral x2 :: Word8)
                         put (fromIntegral y2 :: Word8)


    get = do
        inp <-  get :: Get Word8
        case inp of
            0 -> return StartGame
            1 -> return RotatePlayer
            2 -> do
                  x1 <- get :: Get Word8
                  y1 <- get :: Get Word8
                  x2 <- get :: Get Word8
                  y2 <- get :: Get Word8
                  return $ Move (f x1, f y1) (f x2, f y2)
            3 -> do
                  x1 <- get :: Get Word8
                  y1 <- get :: Get Word8
                  return $ Coord (f x1, f y1)

        where f = fromIntegral


-- | Colors used for checkers and squares
red = RGB 255 0 0
green = RGB 0 255 0
blue = RGB 0 0 255
orange = RGB 255 165 0
yellow = RGB 255 255 0
purple = RGB 153 0 153
white = RGB 255 255 255
black = RGB 0 0 0

startTable :: Table
startTable =                                   [Square (Piece orange) blue (12,0),
                                      Square (Piece orange) blue (11,1), Square (Piece orange) blue (13,1),
                              Square (Piece orange) blue (10,2),Square (Piece orange) blue (12,2), Square (Piece orange) blue (14,2),
                      Square (Piece orange) blue (9,3),Square (Piece orange) blue (11,3), Square (Piece orange) blue (13,3), Square (Piece orange) blue (15,3),Square (Piece green) yellow (0,4),Square (Piece green) yellow (2,4), Square (Piece green) yellow (4,4), Square (Piece green) yellow (6,4), Square Empty white (8,4), Square Empty white (10,4), Square Empty white (12,4), Square Empty white (14,4), Square Empty white (16,4), Square (Piece red) purple (18,4), Square (Piece red) purple (20,4), Square (Piece red) purple (22,4), Square (Piece red) purple (24,4), Square (Piece green) yellow (1,5), Square (Piece green) yellow (3,5), Square (Piece green) yellow (5,5), Square Empty white (7,5), Square Empty white (9,5), Square Empty white (11,5), Square Empty white (13,5), Square Empty white (15,5), Square Empty white (17,5), Square (Piece red) purple (19,5), Square (Piece red) purple (21,5), Square (Piece red) purple (23,5), Square (Piece green) yellow (2,6), Square (Piece green) yellow (4,6), Square Empty white (6,6), Square Empty white (8,6), Square Empty white (10,6), Square Empty white (12,6), Square Empty white (14,6), Square Empty white (16,6), Square Empty white (18,6), Square (Piece red) purple (20,6), Square (Piece red) purple (22,6), Square (Piece green) yellow (3,7), Square Empty white (5,7), Square Empty white (7,7), Square Empty white (9,7), Square Empty white (11,7), Square Empty white (13,7), Square Empty white (15,7), Square Empty white (17,7), Square Empty white (19,7), Square (Piece red) purple (21,7), Square Empty white (4,8), Square Empty white (6,8), Square Empty white (8,8), Square Empty white (10,8), Square Empty white (12,8), Square Empty white (14,8), Square Empty white (16,8), Square Empty white (18,8), Square Empty white (20,8), Square (Piece purple) red (3,9), Square Empty white (5,9), Square Empty white (7,9), Square Empty white (9,9), Square Empty white (11,9), Square Empty white (13,9), Square Empty white (15,9), Square Empty white (17,9), Square Empty white (19,9), Square (Piece yellow) green (21,9), Square (Piece purple) red (2,10), Square (Piece purple) red (4,10), Square Empty white (6,10), Square Empty white (8,10), Square Empty white (10,10), Square Empty white (12,10), Square Empty white (14,10), Square Empty white (16,10), Square Empty white (18,10), Square (Piece yellow) green (20,10), Square (Piece yellow) green (22,10), Square (Piece purple) red (1,11), Square (Piece purple) red (3,11), Square (Piece purple) red (5,11), Square Empty white (7,11), Square Empty white (9,11), Square Empty white (11,11), Square Empty white (13,11), Square Empty white (15,11), Square Empty white (17,11), Square (Piece yellow) green (19,11), Square (Piece yellow) green (21,11), Square (Piece yellow) green (23,11), Square (Piece purple) red (0,12), Square (Piece purple) red (2,12), Square (Piece purple) red (4,12), Square (Piece purple) red (6,12), Square Empty white (8,12), Square Empty white (10,12), Square Empty white (12,12), Square Empty white (14,12), Square Empty white (16,12), Square (Piece yellow) green (18,12), Square (Piece yellow) green (20,12), Square (Piece yellow) green (22,12), Square (Piece yellow) green (24,12), Square (Piece blue) orange (9,13), Square (Piece blue) orange (11,13), Square (Piece blue) orange (13,13), Square (Piece blue) orange (15,13), Square (Piece blue) orange (10,14), Square (Piece blue) orange (12,14), Square (Piece blue) orange (14,14), Square (Piece blue) orange (11,15), Square (Piece blue) orange (13,15), Square (Piece blue) orange (12,16)]
