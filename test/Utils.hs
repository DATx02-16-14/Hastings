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

--Test that checks that the order of the updated list is preserved.
prop_updateLookup_correctOrder :: Int -> [(Int, Int)] -> Bool
prop_updateLookup_correctOrder find list =
  case lookup find list of
    Just val -> length list == length result && length filteredList == 1
    Nothing  -> True

    where
      result = updateLookup updateFun find list
      zipList = zip result list
      filteredList = filter (uncurry (/=)) zipList
      updateFun v = v + 1