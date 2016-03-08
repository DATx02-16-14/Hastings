import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Utils

main = defaultMain tests

tests = [
    testGroup "UpdateLookup" [
      testProperty "Test that exactly one element is updated and that element is updated correctly" prop_updateLookup_correctUpdate,
      testProperty "Test that the list is the same order as before" prop_updateLookup_correctOrder
    ],
    testGroup "UpdateListElem" [
      testProperty "Test that exactly one element is updated and that element is updated correctly" prop_updateListElem_correctUpdate,
      testProperty "Test that the list is the same order as before" prop_updateLookup_correctUpdate
    ]
  ]
