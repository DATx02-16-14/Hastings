import Data.Sequence
import Data.Foldable

type Table = [Square]
data Color = Blue | Red | Pink | Green | Black | Yellow | White
data Content = Empty | Player Color
data Square = Square Contend Color Coord 

type Coord = (Int,Int)
startTable :: Table
startTable =                                    [(Player Black, Blue, (12,0)), 
                                        (Player Black, Blue, (11,1)), (Player Black, Blue, (13,1))
                                (Player Black, Blue, (10,2)),(Player Black, Blue, (12,2)), (Player Black, Blue, (14,2))
                        (Player Black, Blue, (9,3)),(Player Black, Blue, (11,3)), (Player Black, Blue, (13,3)), (Player Black, Blue, (15,3))
                (Player Black, Blue, (8,4)),(Player Black, Blue, (10,4)), (Player Black, Blue, (12,4)), (Player Black, Blue, (14,4)),(Player Black, Blue, (16,4)),
(Player Green, Yellow, (0,5)),(Player Green, Yellow, (2,5)), (Player Green, Yellow, (4,5)), (Player Green, Yellow, (6,5)), (Empty, White,  (8,5)), (Empty, White, (10,5)), (Empty, White, (12,5)), (Empty, White, (14,5)), (Empty, White, (16,5)), (Player Red, Pink, (18,5)), (Player Red, Pink, (20,5)), (Player Red, Pink, (22,5)), (Player Red, Pink, (24,5))]


gameLoop = undefined
