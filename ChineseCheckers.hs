module ChineseCheckers where

import Table


testTable :: Table
testTable = [(Square Empty Red (1,0)),(Square (Piece Blue) Black (2,0)),(Square Empty Blue (0,1))]

squareContent :: Table -> Coord -> Content
squareContent [] _         = error "Table does not contain coordinate"
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

{-
isReachable :: Coord -> Table -> Table
isReachable c t = filter (isReachable' c) t

isReachable' :: Coord -> Square -> Bool
isReachable' (x,y) (Square _ _ (x1,y1)) = (abs(x-x1) == 2 && abs(y-y1) == 0) || (abs(x-x1) == 1 && abs(y-y1) == 1) || (abs(x-x1) == 2 && abs(y-y1) == 2) || (abs(x-x1) == 4 && abs(y-y1) == 0)
-}

-- | Given a coordinate, canMove generates a list of all possible movable squares from that position
canMove :: Coord -> Table -> Table
canMove (x,y) t = filter (checkusPrimus (x,y)) $ filter isEmpty t

    where checkusPrimus (x,y) (Square c _ (x1,y1)) | (x+4) == x1 && y1 == y && (content (x+2,y) == Empty) = False
                                                   | (x-4) == x1 && y1 == y && (content (x-2,y) == Empty) = False
                                                   | (x-2) == x1 && (y+2) == y1 && (content (x-1,y+1)== Empty) = False
                                                   | (x+2) == x1 && (y+2) == y1 && (content (x+1,y+1) == Empty) = False
                                                   | (x+2) == x1 && (y-2) == y1 && (content (x+1,y-1) == Empty) = False
                                                   | (x-2) == x1 && (y-2) == y1 && (content (x-1,y-1) == Empty) = False
                                                   | otherwise = True
          content = squareContent t


movePlayer :: Coord -> Coord -> Table -> Table
movePlayer c1 c2 t | elem c2 $ map coordinates (canMove c1 t) = movePiece t c1 c2
                   | otherwise = error "Can't move"

coordinates :: Square -> Coord
coordinates (Square _ _ coord) = coord

-- | Takes a square and checks if the piece on it is "home", meaning on the square with same color
pieceHome :: Square -> Bool
pieceHome (Square content col _) = case content of 
                                         Piece c -> c == col
                                         _       -> False

-- | Checks if a player has all pieces "home"
playerHome :: Color -> Table -> Bool
playerHome c1 t = and $ map pieceHome $ filter playerOnly t

    where playerOnly (Square cont _ _) = case cont of
                                           Piece col -> col == c1
                                           otherwise -> False

-- | Game is over when all pieces have reached their "home"
gameOver :: Table -> Bool
gameOver t = and $ map pieceHome t


isEmpty :: Square -> Bool
isEmpty c = case c of 
                (Square Empty _ _) -> True
                otherwise -> False


getCoord :: IO (Coord,Coord)
getCoord = do 
            putStrLn "From which coordinate do you want to move?"
            coord <- getLine
            putStrLn "To which coordinate do you want to move?"
            coord2 <- getLine
            return (read coord :: (Int,Int), read coord2 :: (Int,Int))


checkPlayer :: Color -> Content -> Bool
checkPlayer c Empty = False
checkPlayer c (Piece c1) = c == c1

playerMove :: Color -> Table -> IO Table
playerMove col t = do
                (c1,c2) <- getCoord
                case (checkPlayer col (squareContent t c1)) of 
                                          False -> error "Not your piece!!"
                                          True -> return $ movePlayer c1 c2 t 

startGame :: [String] -> IO ()
startGame player = case (length player) of 
                      2         -> startGame' startTable $ zipWith mkPlayer player [Blue,Red]
                      4         -> startGame' startTable $ zipWith mkPlayer player [Blue,Red,Pink,Green]
                      6         -> startGame' startTable $ zipWith mkPlayer player [Blue,Red,Pink,Green,Black,Yellow]
                      otherwise -> error "Not correct number of players for game to start"
              where mkPlayer a b = (a,b)

gameLoop :: Color -> Table -> IO Table
gameLoop c t = playerMove c t  

startGame' :: Table -> [(String,Color)] -> IO ()
startGame' t ((s,col):xs) | gameOver t = putStrLn "GAME OVER!"
                          | otherwise = do 
                                      putStrLn $ s ++ "s speltur"
                                      newTable <- playerMove col t
                                      putStrLn (show newTable)
                                      startGame' newTable $ xs ++ [(s,col)]
