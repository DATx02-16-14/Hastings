module ChineseCheckers where

import Table
import Haste.Graphics.Canvas

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


-- | Checks if a player jumped over a piece, then it should be able to move again
moveAgain :: Coord -> Coord -> Bool
moveAgain (x,y) (x1,y1) = (abs(x1-x) == 4 && abs(y1-y) == 0) || (abs(x1-x) == 2 && abs(y1-y) == 2)


-- | Same as movePlayer but with Maybe type
movePlayer' :: Coord -> Coord -> Table -> Maybe Table
movePlayer' c1 c2 t | elem c2 $ map coordinates (canMove c1 t) = Just $ movePiece t c1 c2
                    | otherwise = Nothing


-- | 
movePlayer :: Coord -> Coord -> Table -> Table
movePlayer c1 c2 t | elem c2 $ map coordinates (canMove c1 t) = movePiece t c1 c2
                   | otherwise = error "Can't move"

-- | Returns coordinates of a square
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


-- | Checks if a square has Empty content
isEmpty :: Square -> Bool
isEmpty c = case c of 
                (Square Empty _ _) -> True
                otherwise -> False


-- | Takes the current state of the game and input coordinates, performing a player action
playerAction :: GameState -> Coord -> GameState
playerAction gs c1 = case fromCoord gs of 
                        Nothing -> GameState {gameTable = gameTable gs
                                             , currentPlayer = currentPlayer gs
                                             , players = players gs
                                             , fromCoord = (Just c1)
                                             , playerMoveAgain = playerMoveAgain gs}

                        Just c2 -> case action gs c1 c2 (playerMoveAgain gs) of
                                (Nothing,_) -> gs
                                (Just table,b) -> case b of
                                    False ->             GameState {gameTable = table
                                                        , currentPlayer = fst . head . tail $ players gs
                                                        , players = (tail $ players gs) ++ [head $ players gs]
                                                        , fromCoord = Nothing
                                                        , playerMoveAgain = b}

                                    True  ->            GameState {gameTable = table
                                                        , currentPlayer = fst . head $ players gs
                                                        , players = players gs
                                                        , fromCoord = Nothing
                                                        , playerMoveAgain = b}

-- | Helper function for playerAction
action :: GameState -> Coord -> Coord -> Bool -> (Maybe Table, Bool)
action gs c1 c2 b = case checkPlayer (color $ head (players gs)) (squareContent (gameTable gs) c1) of
                        False -> (Nothing,False)
                        True | (b && moveAgain c1 c2) -> (movePlayer' c1 c2 (gameTable gs), moveAgain c1 c2)
                             | (b && not (moveAgain c1 c2)) -> (Nothing, False)
                             | otherwise -> (movePlayer' c1 c2 (gameTable gs), moveAgain c1 c2)

--movePlayer' c1 c2 (gameTable gs)
    where color (s,c) = c


{-|
  Given a list of player names, initGame associates each player 
  with a color and generates the inital game state
-}
initGame :: [String] -> GameState
initGame players = case (length players) of 
                      2         -> create $ zipWith mkPlayer players [blue,red]
                      4         -> create $ zipWith mkPlayer players [blue,red,purple,green]
                      6         -> create $ zipWith mkPlayer players [blue,red,purple,green,black,yellow]
                      otherwise -> error "Not correct number of players for game to start"

              where mkPlayer a b = (a,b)
                    create p =  GameState {gameTable = startTable
                                         , currentPlayer = "Pelle"
                                         , players = p
                                         , fromCoord = Nothing
                                         , playerMoveAgain = False}



-- | Checks if the piece belogns to the the player, meaning they are of the same color
checkPlayer :: Color -> Content -> Bool
checkPlayer c Empty = False
checkPlayer c (Piece c1) = c == c1


{-|
  The following functions are only used for testing purposes
  They tests the game logic by letting the programmer play the game from stdin/stdout
-}
{-

-- | printing Color
putColor :: Color -> IO ()
putColor = putChar . head . show 

-- | Priting cell
putSquare :: Square -> IO ()
putSquare (Square content c _) = case content of 
                                        Empty -> putColor c
                                        (Piece color) -> putColor color


-- | Get coordinates from stdin
getCoord :: IO (Coord,Coord)
getCoord = do 
            putStrLn "From which coordinate do you want to move?"
            coord <- getLine
            putStrLn "To which coordinate do you want to move?"
            coord2 <- getLine
            return (read coord :: (Int,Int), read coord2 :: (Int,Int))

-- | Get coordinates and moves a piece belonging to the current player
playerMove :: Color -> Table -> Bool -> IO (Table,Bool)
playerMove col t b = do
                (c1,c2) <- getCoord
                case (checkPlayer col (squareContent t c1)) of 
                                          False -> error "Not your piece!!"
                                          True | (b && moveAgain c1 c2) -> return $ (movePlayer c1 c2 t, moveAgain c1 c2)
                                               | (b && not (moveAgain c1 c2)) -> error "Illegal move!"
                                               | otherwise -> return $ (movePlayer c1 c2 t, moveAgain c1 c2)

-- | Takes a list of player names and creates a new game
startGame :: [String] -> IO ()
startGame player = case (length player) of 
                      2         -> startGame' startTable $ zipWith mkPlayer player [Blue,Red]
                      4         -> startGame' startTable $ zipWith mkPlayer player [Blue,Red,Pink,Green]
                      6         -> startGame' startTable $ zipWith mkPlayer player [Blue,Red,Pink,Green,Black,Yellow]
                      otherwise -> error "Not correct number of players for game to start"
              where mkPlayer a b = (a,b)


-- | Game loop 
startGame' :: Table -> [(String,Color)] -> IO ()
startGame' t ((s,col):xs) | gameOver t = putStrLn "GAME OVER!"
                          | otherwise = do 
                                      putStrLn $ s ++ "s speltur"
                                      (newTable,again) <- playerMove col t False
                                      putStrLn (show newTable)
                                      case again of 
                                        True -> jumpAgain newTable $ ((s,col):xs)
                                        False -> startGame' newTable $ xs ++ [(s,col)]

-- | Help function for the game loop, for when a player can move again
jumpAgain :: Table -> [(String,Color)] -> IO ()
jumpAgain t ((s,col):xs) = do 
                            putStrLn $ s ++ "s speltur!"
                            (newTable,again) <- playerMove col t True
                            case again of
                              True -> jumpAgain newTable ((s,col):xs)
                              _    -> startGame' newTable $ xs ++ [(s,col)]

-}