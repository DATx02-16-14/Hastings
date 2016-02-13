module Main
    where
import Haste.App
import Haste.App.Standalone
import Lobby
import qualified Control.Concurrent as CC
import Haste.Events
import Haste.DOM

main :: IO ()
main = runStandaloneApp $ do
      playersList <- liftServerIO $ CC.newMVar []
      gamesList <- liftServerIO $ CC.newMVar []

      handshake <- remote $ srvHandshake playersList
      createGame <- remote $ srvCreateGame gamesList playersList
      onSessionEnd $ srvCloseConnection playersList

      runClient $ do
         liftIO createLobbyDOM

         name <- prompt "Hello! Please enter your name:"
         onServer $ handshake <.> name

         _ <- ($)
            withElems ["createGamebtn"] $ \[createGamebtn] ->
               onEvent createGamebtn Click $ \(MouseData _ mb _) ->
                  case mb of
                     Just MouseLeft -> do
                        onServer createGame
                     _ -> return ()


         return ()
