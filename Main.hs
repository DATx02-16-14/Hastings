
import Haste.App

main :: IO ()
main = do
  runApp (mkConfig "localhost" 24601) $ do
    undefined

    runClient $ do
      undefined
