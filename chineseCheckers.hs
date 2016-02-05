
type Table = [Square]
data Color = Blue | Red | Pink | Green | Black | Yellow | White
<<<<<<< HEAD
data Content = Empty | Piece Color
data Square = Square Content Color Coord
=======
 deriving (Show, Eq)

data Content = Empty | Piece Color
 deriving (Show)

data Square = Square Content Color Coord
 deriving (Show)
>>>>>>> db1a8b2ce6f43f5248312160f298f6c210acb712

type Coord = (Int,Int)


startTable :: Table

startTable =                                    [(Square (Piece Black) Blue (12,0)), 
                                        (Square (Piece Black) Blue (11,1)), (Square (Piece Black) Blue (13,1)),
                                (Square (Piece Black) Blue (10,2)),(Square (Piece Black) Blue (12,2)), (Square (Piece Black) Blue (14,2)),
                        (Square (Piece Black) Blue (9,3)),(Square (Piece Black) Blue (11,3)), (Square (Piece Black) Blue (13,3)), (Square (Piece Black) Blue (15,3)),(Square (Piece Green) Yellow (0,4)),(Square (Piece Green) Yellow (2,4)), (Square (Piece Green) Yellow (4,4)), (Square Empty White (6,4)), (Square Empty White (8,4)), (Square Empty White (10,4)), (Square Empty White (12,4)), (Square Empty White (14,4)), (Square Empty White (16,4)), (Square (Piece Red) Pink (18,4)), (Square (Piece Red) Pink (20,4)), (Square (Piece Red) Pink (22,4)), (Square (Piece Red) Pink (24,4)), (Square (Piece Green) Yellow (1,5)), (Square (Piece Green) Yellow (3,5)), (Square (Piece Green) Yellow (5,5)), (Square Empty White (7,5)), (Square Empty White (9,5)), (Square Empty White (11,5)), (Square Empty White (13,5)), (Square Empty White (15,5)), (Square Empty White (17,5)), (Square (Piece Red) Pink (19,5)), (Square (Piece Red) Pink (21,5)), (Square (Piece Red) Pink (23,5)), (Square (Piece Green) Yellow (2,6)), (Square (Piece Green) Yellow (4,6)), (Square Empty White (6,6)), (Square Empty White (8,6)), (Square Empty White (10,6)), (Square Empty White (12,6)), (Square Empty White (14,6)), (Square Empty White (16,6)), (Square Empty White (18,6)), (Square (Piece Red) Pink (20,6)), (Square (Piece Red) Pink (22,6)), (Square (Piece Green) Yellow (3,7)), (Square Empty White (5,7)), (Square Empty White (7,7)), (Square Empty White (9,7)), (Square Empty White (11,7)), (Square Empty White (13,7)), (Square Empty White (15,7)), (Square Empty White (17,7)), (Square Empty White (19,7)), (Square (Piece Red) Pink (21,7)), (Square Empty White (4,8)), (Square Empty White (6,8)), (Square Empty White (8,8)), (Square Empty White (10,8)), (Square Empty White (12,8)), (Square Empty White (14,8)), (Square Empty White (16,8)), (Square Empty White (18,8)), (Square Empty White (20,8)), (Square (Piece Pink) Red (3,9)),  (Square Empty White (5,9)), (Square Empty White (7,9)), (Square Empty White (9,9)), (Square Empty White (11,9)), (Square Empty White (13,9)), (Square Empty White (15,9)), (Square Empty White (17,9)), (Square Empty White (19,9)), (Square (Piece Yellow) Green (21,9)), (Square (Piece Pink) Red (2,10)), (Square (Piece Pink) Red (4,10)), (Square Empty White (6,10)), (Square Empty White (8,10)), (Square Empty White (10,10)), (Square Empty White (12,10)), (Square Empty White (14,10)), (Square Empty White (16,10)), (Square Empty White (18,10)), (Square (Piece Yellow) Green (20,10)), (Square (Piece Yellow) Green (22,10)), (Square (Piece Pink) Red (1,11)), (Square (Piece Pink) Red (3,11)), (Square (Piece Pink) Red (5,11)), (Square Empty White (7,11)), (Square Empty White (9,11)), (Square Empty White (11,11)), (Square Empty White (13,11)), (Square Empty White (15,11)), (Square Empty White (17,11)), (Square (Piece Yellow) Green (19,11)), (Square (Piece Yellow) Green (21,11)), (Square (Piece Yellow) Green (23,11)), (Square (Piece Pink) Red (0,12)), (Square (Piece Pink) Red (2,12)), (Square (Piece Pink) Red (4,12)), (Square (Piece Pink) Red (6,12)), (Square Empty White (8,12)), (Square Empty White (10,12)), (Square Empty White (12,12)), (Square Empty White (14,12)), (Square Empty White (16,12)), (Square (Piece Yellow) Green (18,12)), (Square (Piece Yellow) Green (20,12)), (Square (Piece Yellow) Green (22,12)), (Square (Piece Yellow) Green (24,12)), (Square (Piece Blue) Black (9,13)), (Square (Piece Blue) Black (11,13)), (Square (Piece Blue) Black (13,13)), (Square (Piece Blue) Black (15,13)), (Square (Piece Blue) Black (10,14)), (Square (Piece Blue) Black (12,14)), (Square (Piece Blue) Black (14,14)), (Square (Piece Blue) Black (11,15)), (Square (Piece Blue) Black (11,15)), (Square (Piece Blue) Black (12,16))] 


testTable :: Table
testTable = [(Square Empty Red (1,0)),(Square (Piece Blue) Black (2,0)),(Square Empty Blue (0,1))]

squareContent :: Table -> Coord -> Content
squareContent (t:ts) (x,y) = case t of
                                Square content _ coord | check coord -> content
                                                       | otherwise -> squareContent ts (x,y) 

            where check (x1,y1) = x1 == x && y1 == y


-- | puts a checker on the table
putPiece :: Table -> Content -> Coord -> Table
putPiece (t:ts) c (x,y) = case t of
                          (Square _ color coord) | check coord -> (Square c color coord):ts
                                                 | otherwise -> t:(putPiece ts c (x,y))

             where check (x1,y1) = x1 == x && y1 == y


-- | removes a checker from the table
removePiece :: Table -> Coord -> Table
removePiece (t:ts) (x,y) = case t of 
                        (Square _ color coord) | check coord -> (Square Empty color coord):ts
                                               | otherwise -> t:(removePiece ts (x,y))
>>>>>>> db1a8b2ce6f43f5248312160f298f6c210acb712

             where check (x1,y1) = x1 == x && y1 == y

-- | moves a piece from its original positon to the new
movePiece :: Table -> Coord -> Coord -> Table
movePiece t (x1,y1) (x2,y2) = removePiece (putPiece t content (x2,y2)) (x1,y1)
   where content = squareContent t (x1,y1)


-- | removes a player (color) from the table
removePlayer ::  Color -> Table -> Table
removePlayer c = filter (isPlayer)

       where isPlayer (Square content _ _) = case content of
                                            Empty -> True
                                            Piece color -> color == c



-- | printing Color
putColor :: Color -> IO ()
putColor = putChar . head . show 

-- | Priting cell
putSquare :: Square -> IO ()
putSquare (Square content c _) = case content of 
                                        Empty -> putColor c
                                        (Piece color) -> putColor color

gameLoop = undefined



