import ChineseCheckers
import Table
import Test.QuickCheck

newtype Test = Test (Table, Content, (Int,Int))
        deriving (Show)

newtype OnlyPiece = OnlyPiece Content
        deriving (Show)

instance Arbitrary OnlyPiece where
  arbitrary = do
            con   <- arbitrary :: Gen Color
            return $ OnlyPiece (Piece con)

instance Arbitrary Test where
  arbitrary = do
            con   <- arbitrary :: Gen Content
            table <- listOf1 $ arbitrary :: Gen Table
            coord <-  elements $ map getCoord' table
            return $ Test (table,con,coord)
            
getCoord' :: Square -> Coord
getCoord' (Square _ _ c) = c


instance Arbitrary Color where
  arbitrary = elements [Blue, Red, Pink, Green, Black, Yellow, White]


instance Arbitrary Content where
    arbitrary = do
            col <- arbitrary
            frequency [(1, return Empty),(4,return (Piece col))]


instance Arbitrary Square where
    arbitrary = do 
            content <- arbitrary :: Gen Content
            color <- arbitrary :: Gen Color
            x <- arbitrary :: Gen Int
            y <- arbitrary :: Gen Int
            return $ (Square content color (x,y))

prop_removePiece :: Test -> Bool
prop_removePiece (Test (t, c, coord)) = squareContent (removePiece (putPiece t c coord) coord) coord == Empty

prop_putPiece :: Test -> OnlyPiece -> Bool
prop_putPiece (Test (t, _, coord)) (OnlyPiece (p)) = squareContent (putPiece t p coord) coord /= Empty

--tests the start table if all squares with no pieces are white 
testStartTable :: Table -> Bool
testStartTable xs
                  |testStartTable' xs > 1 = False
                  |otherwise = True

testStartTable' :: Table -> Int
testStartTable' [] = 0
testStartTable' ((Square (Piece _) color _):xs) =  testStartTable' xs
testStartTable' ((Square Empty color _):xs)
                                   |color == White = testStartTable' xs
                                   |otherwise      = 1 + testStartTable' xs


