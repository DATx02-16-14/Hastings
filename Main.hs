
import Haste.App
import Haste.App.Standalone

main :: IO ()
main = runStandaloneApp $ do

    runClient $ do
      return ()
