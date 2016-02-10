import ChineseCheckers
import Table
import Test.QuickCheck


cell :: Gen Content
cell = elements [Piece x | x <- [Blue, Red, Pink, Green, Black, Yellow, White]]
instance Arbitrary Color where
  arbitrary = elements [Blue, Red, Pink, Green, Black, Yellow, White]

--instance Arbitrary Content where
-- arbitrary = do
--     Color x <- arbitrary
--     return $ choose (Empty, Piece Color)

--instance Arbitrary Square where
--  arbitrary = choose (Square )

instance Arbitrary Content where
    arbitrary = do
            col <- arbitrary
            elements [Empty, Piece col]


instance Arbitrary Square where
    arbitrary = do 
            content <- arbitrary :: Gen Content
            color <- arbitrary :: Gen Color
            n1 <- arbitrary :: Gen Int
            n2 <- arbitrary :: Gen Int
            return $ (Square content color (n1,n2))

prop_putPiece' :: Table -> Content -> (Int,Int) -> Property
prop_putPiece' t c coord = and [length t > 0, elem coord (map coordinates t)] ==> squareContent (removePiece (putPiece t c coord) coord) coord == Empty 


prop_putPiece :: Table -> Content -> (Int,Int) -> Bool
prop_putPiece xs c (x,y) = squareContent (putPiece xs c (x,y)) (x,y) == c
               -- where types = xs :: Table, c :: Content

--tests the start table if all squares are white 
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


