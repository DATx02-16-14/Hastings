module Main
    where
import Haste.App
import Haste.App.Standalone
import Lobby
import qualified Control.Concurrent as CC

main :: IO ()
main = runStandaloneApp $ do
      playersList <- liftServerIO $ CC.newMVar []

      handshake <- remote $ srvHandshake playersList
      onSessionEnd $ srvCloseConnection playersList

      runClient $ do
        name <- prompt "Hello! Please enter your name:"
        onServer $ handshake <.> name
        return ()
