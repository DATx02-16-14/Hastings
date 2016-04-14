module ChineseCheckers.ChineseBitmaps where

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import ChineseCheckers.Table

filepath :: String
filepath = "file:////home/benjamin/Documents/"

renderTest can = do
      bitmap <- loadBitmap "file:////home/michael/Documents/cooltext170130995424459.gif"
      renderOnTop can $ draw bitmap (10,10)

-- | Render the border
renderTable can  = do
        bitmap <- loadBitmap $ filepath ++ "0305509001456402835_chinese_checkers_start_posit.png"
        renderOnTop can $ scale (1.5,0.90) $ draw bitmap (0,20)

-- | Render a high-lighted piece to the specified canvas, color and coordinate. See also renderSquare
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


-- | Render a piece to the specified canvas, color and coordinate.
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
