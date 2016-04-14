import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Utils
import Hastings.ServerUtilsTests
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
    testGroup "getUUIDFromGamesList" [
      testProperty "Tests that the uuids received are the correct ones" prop_getUUIDFromGamesList
    ],
    testGroup "deletePlayerFromGame" [
      testProperty "Checks that a player has been deleted after the function is done" prop_deletePlayerFromGame_length
    ],
    testGroup "addPlayerToGame" [
      testProperty "Checks that only a single player has been added" prop_addPlayerToGame_length,
      testProperty "Checks that only one LobbyGame has been changed" prop_addPlayerToGame_unique
    ],
    testGroup "findGameWithID" [
      testProperty "Check that the correct game has been found" prop_findGameWithID
    ],
    testGroup "isOwnerOfGame" [
      testProperty "Checks that the function returns the correct result (True if the client is last in the players list, False otherwise)" prop_isOwnerOfGame
    ],
    testGroup "Server.Lobby" [
      testProperty "Checks that connect successfully adds a client" prop_connect ,
      testProperty "Checks that disconnect successfully disconnects a player from both games and lobby" prop_disconnect,
      testProperty "Checks that the list of player names is correct" prop_getConnectedPlayerNames,
      testProperty "Checks that the name of the player is changed everywhere" prop_changeNickName
    ]

  ]
