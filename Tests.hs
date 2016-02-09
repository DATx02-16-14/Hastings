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


