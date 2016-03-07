import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Utils

main = defaultMain tests

tests = [
        testGroup "UpdateLookup" [
                testProperty "Element Updated" prop_updateLookup
            ]
    ]
