import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))


import Utils
import Server.LobbyTest
import Server.GameTest


mainWithOpts = do
  -- Test options can also be specified in the code. The TestOptions
  -- type is an instance of the Monoid type class, so the easiest way
  -- to get an empty set of options is with `mempty`.
  let empty_test_opts = mempty :: TestOptions

  -- We update the empty TestOptions with our desired values.
  let my_test_opts = empty_test_opts {
    topt_maximum_generated_tests = Just 100
  }

  -- Now we create an empty RunnerOptions in the same way, and add
  -- our TestOptions to it.
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts,
    -- Run tests on 1 thread only to prevent race conditions on the database.
    ropt_threads = Just 1
  }

  defaultMainWithOpts tests my_runner_opts


main = mainWithOpts

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
    ],
    testGroup "Server.Game" [
      testProperty "Checks that leaveGame removes the correct player" prop_leaveGame,
      testProperty "Checks that playerJoinGame correctly adds a player" prop_joinGame,
      testProperty "Checks that a game can be properly created" prop_createGame
    ]

  ]
