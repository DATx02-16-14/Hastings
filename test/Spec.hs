import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Utils

main = defaultMain tests

tests = [
    testGroup "UpdateLookup" [
      testProperty "Test that exactly one element is updated and that element is updated correctly" prop_updateLookup_correctUpdate
    ]
  ]
