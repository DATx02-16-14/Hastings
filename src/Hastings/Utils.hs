-- |Module for all of the util functions that does not belong elsewhere and are perhaps so generic that they could be made into a separate library
module Hastings.Utils where

import Data.Maybe

-- |Method that finds with IO and then returns the value
-- together with the list to the right and left of the element
findIO :: (a -> IO Bool) -> [a] -> IO (Maybe a, [a], [a])
findIO p as = findIO' p as []
  where
    findIO' _ [] hs = return (Nothing, [], hs)
    findIO' p (a:as) hs = do
      b <- p a
      if b
        then
          return (Just a, hs, as)
        else
          findIO' p as (a:hs)

-- |Updates the value of the first occurance of the key in the given table
-- Takes a function which takes the previous value and returns the new
updateLookup :: Eq a => (b -> b) -> a -> [(a,b)] ->  [(a,b)]
updateLookup f k t = updateLookup' [] k t f
  where
    updateLookup' checkedEs _ [] _ = reverse checkedEs
    updateLookup' checkedEs k (e@(k',v):es) f
            | k == k'   = reverse checkedEs ++ [(k, f v)] ++ es
            | otherwise = updateLookup' (e:checkedEs) k es f
