module Utils where

import Data.Maybe
import Data.List
import Test.QuickCheck.Property
import Hastings.Utils

--Test that checks that exactly one element in the list has been updated and that the updated value is correct
prop_updateLookup_correctUpdate :: Int -> [(Int, Int)] -> Property
prop_updateLookup_correctUpdate element list = isJust value ==>
     length resultDiff == 1 && snd (head resultDiff) == updateFun (fromJust value)

  where
    result = updateLookup updateFun element list
    resultDiff = result \\ list
    updateFun v = v + 1
    value = lookup element list

--Test that checks that the order of the updated list is preserved.
prop_updateLookup_correctOrder :: Int -> [(Int, Int)] -> Property
prop_updateLookup_correctOrder element list = isJust value ==>
    length list == length result && length filteredList == 1

    where
      result = updateLookup updateFun element list
      zipList = zip result list
      filteredList = filter (uncurry (/=)) zipList
      updateFun v = v + 1
      value = lookup element list