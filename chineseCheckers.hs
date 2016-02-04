import Data.Sequence
import Data.Foldable

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
--insert2Dlist (x,y) a xs = (fst(Prelude.splitAt (y+1) xs)) ++ [replaceAt x a (Prelude.concat (snd(Prelude.splitAt (y+1) xs)))] ++ (snd(Prelude.splitAt (y+1) xs))

insert2Dlist (x,y) a xs = (fst(Prelude.splitAt (y+1) xs)) ++ replaceAt x a head . snd(Prelude.splitAt (y+1) xs) ++ (snd(Prelude.splitAt (y+1) xs))

move :: Table -> (Int, Int) -> (Int, Int) -> Table
move t (x1,y1) (x2, y2) = undefined

getSquare :: Table -> (Int,Int) -> Square
getSquare t (x,y) = (t !! y) !! x

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i a xs = toList .  update i a $ fromList xs
                        

gameLoop = undefined
