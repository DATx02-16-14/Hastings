import ChineseCheckers

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
