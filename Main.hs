module Main
    where
import Haste.App
import Haste.App.Standalone
import Haste.DOM
import Haste.Events
import Lobby
import qualified Control.Concurrent as CC

main :: IO ()
main = runStandaloneApp $ do
      playersList <- liftServerIO $ CC.newMVar []
      gamesList <- liftServerIO $ CC.newMVar []

      handshake <- remote $ srvHandshake playersList
      getGamesList <- remote $ srvGetGamesList gamesList
      joinGame <- remote $ srvPlayerJoinGame playersList gamesList
      onSessionEnd $ srvCloseConnection playersList

      runClient $ do
        name <- prompt "Hello! Please enter your name:"
        onServer $ handshake <.> name

        gameList <- onServer getGamesList
        mapM_ (addGameToDOM joinGame) gameList

        return ()


addGameToDOM :: Remote (String -> Server ()) -> String -> Client ()
addGameToDOM joinGame gameName = do
  gameDiv <- newElem "div"
  gameEntry <- newElem "button" `with`
    [
      prop "id" =: gameName
    ]
  textElem <- newTextElem gameName
  appendChild gameEntry textElem
  appendChild gameDiv gameEntry
  appendChild documentBody gameDiv

  _ <- ($)
    withElems [gameName] $ \[gameButton] ->
      onEvent gameButton Click (\(MouseData _ mb _) ->
        case mb of
          Just MouseLeft ->
            onServer $ joinGame <.> gameName
          _ -> return ())

  return ()
