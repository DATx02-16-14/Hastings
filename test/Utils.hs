module Utils where

import Data.Maybe
import Data.List
import Test.QuickCheck.Property
import Hastings.Utils

--Test that checks that exactly one element in the list has been updated and that the updated value is correct
prop_updateLookup_correctUpdate :: Int -> [(Int, Int)] -> Property
prop_updateLookup_correctUpdate element list = isJust value ==>
     length resultDiff == 1 && snd (head resultDiff) == updateFunction (fromJust value)

  where
    result = updateLookup updateFunction element list
    resultDiff = result \\ list
    value = lookup element list

--Test that checks that the order of the updated list is preserved.
prop_updateLookup_correctOrder :: Int -> [(Int, Int)] -> Property
prop_updateLookup_correctOrder element list = isJust (lookup element list) ==>
    length list == length result && length filteredList == 1

    where
      result = updateLookup updateFunction element list
      filteredList = filter (uncurry (/=)) $ zip result list

prop_updateListElem_correctUpdate :: Int -> [Int] -> Property
prop_updateListElem_correctUpdate element list = elem element list ==>
    length resultDiff == 1 && head resultDiff == updateFunction element

    where
      result = updateListElem updateFunction (updateListElemPredicate element) list
      resultDiff = result \\ list

updateFunction :: Int -> Int
updateFunction value = value + 1

updateListElemPredicate :: Int -> Int -> Bool
updateListElemPredicate expectedValue value = expectedValue == value
