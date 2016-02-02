type Table = [[Square]]
type Pgoal = Int
data Square = Empty Pgoal | Piece Pgoal



startTable :: Table
startTable = [[]]


remove :: Table -> (Int, Int) -> Table
remove = undefined
place :: Table -> (Int, Int) -> (Int, Int) -> Maybe Table
place t (x1,y1) (x2,y2) = case (getSquare t (x2,y2)) of 
                                Empty _ -> Nothing
                                otherwise -> Nothing

place' :: Table -> Square -> (Int,Int) -> Table
place' t s (x,y) = undefined

insert2Dlist :: (Int,Int) -> a -> [[a]] -> [[a]]
insert2Dlist (x,y) a xs =  undefined 


move :: Table -> (Int, Int) -> (Int, Int) -> Table 
move t (x1,y1) (x2, y2) = undefined

getSquare :: Table -> (Int,Int) -> Square
getSquare t (x,y) = (t !! y) !! x

gameLoop = undefined
