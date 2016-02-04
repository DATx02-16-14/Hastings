import Data.Sequence
import Data.Foldable

type Table = [Square]
data Color = Blue | Red | Pink | Green | Black | Yellow | White
data Piece = Player Color
data Square = Empty Color Coord | Piece Color Coord

type Coord = (Int,Int)

startTable :: Table
startTable =                                    [(Player Black, Blue, (12,0)), 
                                        (Player Black, Blue, (11,1)), (Player Black, Player Blue , (13,1))
                                (Piece, 5, (10,2)),(Piece, 5, (12,2)), (Piece, 5, (14,2))
                        (Piece, 5, (9,3)),(Piece, 5, (11,3)), (Piece, 5, (13,3)), (Piece, 5, (15,3))
                (Piece, 5, (8,4)),(Piece, 5, (10,4)), (Piece, 5, (12,4)), (Piece, 5, (14,4)), (Piece, 5, (16,4))]
(Piece, 4, (0,5)),(Piece, 4, (2,5)), (Piece, 4, (4,5)), (Piece, 4, (6,5)), (Piece, 0, (8,5)), (Piece, 4, (10,5))]


remove :: Table -> (Int, Int) -> Table
remove = undefined
place :: Table -> (Int, Int) -> (Int, Int) -> Maybe Table
place t (x1,y1) (x2,y2) = case (getSquare t (x2,y2)) of 
                                Empty _ -> Nothing
                                otherwise -> Nothing

place' :: Table -> Square -> (Int,Int) -> Table
place' t s (x,y) = undefined




move :: Table -> (Int, Int) -> (Int, Int) -> Table
move t (x1,y1) (x2, y2) = undefined

getSquare :: Table -> (Int,Int) -> Square
getSquare t (x,y) = (t !! y) !! x

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i a xs = toList .  update i a $ fromList xs
                        

gameLoop = undefined
