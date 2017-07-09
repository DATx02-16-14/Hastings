module ChineseCheckers.ChineseBitmaps where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import ChineseCheckers.Table

filepath :: String
filepath = "http://hastings.tejp.xyz"

xShift = 5
yShift = 5
width = 30.0
height = 30.0

widthPiece = 10
heightPiece = 20

renderTest can = do
      bitmap <- loadBitmap $ filepath ++ "/cooltext170130995424459.gif"
      renderOnTop can $ draw bitmap (10,10)

-- | Render the border
renderTable can  = do
        bitmap <- loadBitmap $ filepath ++ "0305509001456402835_chinese_checkers_start_posit.png"
        renderOnTop can $ scale (1.5,0.90) $ draw bitmap (0,20)

-- | Render a high-lighted piece to the specified canvas, color and coordinate. See also renderSquare
renderSquare2 can space size (Piece col) (x,y)
        |col == blue = do
                bitmap <- loadBitmap $ filepath ++ "/blue2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == green = do
                bitmap <- loadBitmap $ filepath ++ "/green3.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == orange = do
                bitmap <- loadBitmap $ filepath ++ "/orange2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == yellow = do
                bitmap <- loadBitmap $ filepath ++ "/yellow2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == purple = do
                bitmap <- loadBitmap $ filepath ++ "/purple2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == red = do
                bitmap <- loadBitmap $ filepath ++ "/red2.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)


-- | Render a piece to the specified canvas, color and coordinate.
renderSquare can space size (Square Empty _ (x,y)) = do
        bitmap <- loadBitmap $ filepath ++ "/empty.bmp"
        renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
renderSquare can space size (Square (Piece col) _ (x,y))
        |col == blue = do
                bitmap <- loadBitmap $ filepath ++ "/blue.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == green = do
                bitmap <- loadBitmap $ filepath ++ "/green.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == orange = do
                bitmap <- loadBitmap $ filepath ++ "/orange.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == yellow = do
                bitmap <- loadBitmap $ filepath ++ "/yellow.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == purple = do
                bitmap <- loadBitmap $ filepath ++ "/purple.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
        |col == red = do
                bitmap <- loadBitmap $ filepath ++ "/red.bmp"
                renderOnTop can $ drawScaled bitmap (Rect (size*fromIntegral x + space*fromIntegral (x+xShift)) (size* fromIntegral y+space* fromIntegral (y+yShift)) widthPiece heightPiece)
