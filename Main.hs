module Main
    where
import Haste.App
import Haste.App.Standalone
import Haste.DOM
import Lobby
import qualified Control.Concurrent as CC

main :: IO ()
main = runStandaloneApp $ do
      playersList <- liftServerIO $ CC.newMVar []
      gamesList <- liftServerIO $ CC.newMVar []

      handshake <- remote $ srvHandshake playersList
      getGamesList <- remote $ srvGetGamesList gamesList
      onSessionEnd $ srvCloseConnection playersList

      runClient $ do
        name <- prompt "Hello! Please enter your name:"
        onServer $ handshake <.> name

        gameList <- onServer getGamesList
        mapM_ addGameToDOM gameList

        return ()


addGameToDOM :: String -> Client ()
addGameToDOM gameName = do
   textElem <- newTextElem gameName
   appendChild documentBody textElem