module Utils where

import Data.Maybe
import Hastings.Utils

--Not very good test that checks that the first value in the list should be updated correctly.
prop_updateLookup :: Int -> [(Int, Int)] -> Bool
prop_updateLookup find list =
  case lookup find list of
    Just val -> (updateFun val) == (fromJust $ lookup find result)
    Nothing  -> True

  where
    result = updateLookup updateFun find list
    updateFun = (\v -> v + 1)
