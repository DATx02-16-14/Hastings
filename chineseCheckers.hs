import Table


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
removePlayer c = map (isPlayer)

       where isPlayer (Square content col (x,y)) = case content of
                                            Empty -> Square content col (x,y)
                                            Piece color | color == c -> Square Empty col (x,y)
                                                        | otherwise -> (Square content col (x,y))



-- | printing Color
putColor :: Color -> IO ()
putColor = putChar . head . show 

-- | Priting cell
putSquare :: Square -> IO ()
putSquare (Square content c _) = case content of 
                                        Empty -> putColor c
                                        (Piece color) -> putColor color

gameLoop = undefined

