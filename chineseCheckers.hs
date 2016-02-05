
type Table = [Square]
data Color = Blue | Red | Pink | Green | Black | Yellow | White
 deriving (Show, Eq)

data Content = Empty | Piece Color
 deriving (Show)

data Square = Square Content Color Coord
 deriving (Show)

type Coord = (Int,Int)

{-
startTable :: Table
startTable =                                    [(Player Black, Blue, (12,0)), 
                                        (Player Black, Blue, (11,1)), (Player Black, Blue, (13,1))
                                (Player Black, Blue, (10,2)),(Player Black, Blue, (12,2)), (Player Black, Blue, (14,2))
                        (Player Black, Blue, (9,3)),(Player Black, Blue, (11,3)), (Player Black, Blue, (13,3)), (Player Black, Blue, (15,3))
                (Player Black, Blue, (8,4)),(Player Black, Blue, (10,4)), (Player Black, Blue, (12,4)), (Player Black, Blue, (14,4)),(Player Black, Blue, (16,4)),
(Player Green, Yellow, (0,5)),(Player Green, Yellow, (2,5)), (Player Green, Yellow, (4,5)), (Player Green, Yellow, (6,5)), (Empty, White,  (8,5)), (Empty, White, (10,5)), (Empty, White, (12,5)), (Empty, White, (14,5)), (Empty, White, (16,5)), (Player Red, Pink, (18,5)), (Player Red, Pink, (20,5)), (Player Red, Pink, (22,5)), (Player Red, Pink, (24,5))]
-}


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
                                            Piece color -> not $ color == c



-- | printing Color
putColor :: Color -> IO ()
putColor = putChar . head . show 

-- | Priting cell
putSquare :: Square -> IO ()
putSquare (Square content c _) = case content of 
                                        Empty -> putColor c
                                        (Piece color) -> putColor color

gameLoop = undefined

