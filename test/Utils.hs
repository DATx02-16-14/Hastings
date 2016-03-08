module Utils where

import Data.Maybe
import Data.List
import Hastings.Utils

--Test that checks that exactly one element in the list has been updated and that the updated value is correct
prop_updateLookup_correctUpdate :: Int -> [(Int, Int)] -> Bool
prop_updateLookup_correctUpdate find list =
  case lookup find list of
    Just val -> length resultDiff == 1 && snd (head resultDiff) == updateFun val
    Nothing  -> True

  where
    result = updateLookup updateFun find list
    resultDiff = result \\ list
    updateFun v = v + 1
