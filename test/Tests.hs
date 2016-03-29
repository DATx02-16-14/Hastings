import ChineseCheckers
import Table
import Test.QuickCheck
import Haste.Graphics.Canvas

newtype TableCoords = TableCoords (Table, Content, (Int,Int))
        deriving (Show)

newtype OnlyPiece = OnlyPiece Content
        deriving (Show)

newtype TableCoords2 = TableCoords2 (Table, Content, (Int,Int))
        deriving (Show)

instance Arbitrary OnlyPiece where
  arbitrary = do
            con   <- arbitrary :: Gen Color
            return $ OnlyPiece (Piece con)

-- | Generates non-empty tables with a coord that exists in the table
instance Arbitrary TableCoords where
  arbitrary = do
            con   <- arbitrary :: Gen Content
            table <- listOf1 arbitrary :: Gen Table
            coord <- elements $ map getCoord' table
            return $ TableCoords (table,con,coord)

instance Arbitrary TableCoords2 where
  arbitrary = do
            con   <- arbitrary :: Gen Content
            coord <- elements $ map getCoord' startTable
            return $ TableCoords2 (startTable,con,coord)


getCoord' :: Square -> Coord
getCoord' (Square _ _ c) = c


instance Arbitrary Color where
  arbitrary = elements [white, red, blue, green]

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
            return $ Square content color (x,y)

prop_removePiece :: TableCoords -> Bool
prop_removePiece (TableCoords (t, c, coord)) = squareContent (removePiece (putPiece t c coord) coord) coord == Empty

prop_putPiece :: TableCoords -> OnlyPiece -> Bool
prop_putPiece (TableCoords (t, _, coord)) (OnlyPiece p) = squareContent (putPiece t p coord) coord /= Empty

prop_move :: TableCoords2 -> Bool
prop_move (TableCoords2 (t, _, coord)) = all (dist coord) (canMove coord t)

dist :: Coord -> Square -> Bool
dist (x2,y2) (Square _  _ (x1,y1)) = sqrt(fromIntegral(x2-x1)^2 + fromIntegral(y2-y1)^2) <= 4

--test startTable if all squares with no pieces are white 
testStartTable :: Table -> Bool
testStartTable xs
                  |testStartTable' xs > 1 = False
                  |otherwise = True

testStartTable' :: Table -> Int
testStartTable' [] = 0
testStartTable' (Square (Piece _) color _ :xs) =  testStartTable' xs
testStartTable' (Square Empty color _ :xs)
                                   |color == white = testStartTable' xs
                                   |otherwise      = 1 + testStartTable' xs


