import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Utils
import Server.LobbyTest

main = defaultMain tests

tests = [
    testGroup "UpdateLookup" [
      testProperty "Test that exactly one element is updated and that element is updated correctly" prop_updateLookup_correctUpdate,
      testProperty "Test that the list is the same order as before" prop_updateLookup_correctOrder
    ],
    testGroup "UpdateListElem" [
      testProperty "Test that exactly one element is updated and that element is updated correctly" prop_updateListElem_correctUpdate,
      testProperty "Test that the list is the same order as before" prop_updateLookup_correctUpdate
    ],
    testGroup "Server.Lobby" [
      testProperty "Checks that connect successfully adds a client" prop_connect ,
      testProperty "Checks that disconnect successfully disconnects a player from both games and lobby" prop_disconnect,
      testProperty "Checks that the list of player names is correct" prop_getConnectedPlayerNames,
      testProperty "Checks that the name of the player is changed everywhere" prop_changeNickName
    ]

  ]
