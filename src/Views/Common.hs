module Views.Common
  where

import Haste (Interval(Once), setTimer)
import Haste.App
import Haste.DOM
import Haste.Events

import LobbyTypes
import LobbyAPI

import Data.Maybe

initDOM :: Client ()
initDOM = do
  cssLink <- newElem "link" `with`
    [
      prop "rel"          =: "stylesheet",
      prop "type"         =: "text/css",
      prop "href"         =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
    --prop "integrity"    =: "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7",
      prop "crossorigin"  =: "anonymous"
    ]

  appendChild documentBody cssLink

createBootstrapTemplate :: String -> Client Elem
createBootstrapTemplate parentName = do

  containerDiv <- newElem "div" `with`
    [
      attr "class" =: "container-fluid",
      attr "id"    =: "container-fluid"
    ]

  rowDiv <- newElem "div" `with`
    [
      attr "class" =: "row",
      attr "id"    =: "row"
    ]

  leftPaddingColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-3",
      attr "id"    =: "leftContent"
    ]
  rightPaddingColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-3",
      attr "id"    =: "rightContent"
    ]

  centerColDiv <- newElem "div" `with`
    [
      attr "class" =: "col-md-6",
      attr "id"    =: "centerContent"
    ]
  parentDiv <- newElem "div" `with`
    [
      prop "id" =: parentName
    ]

  appendChild documentBody containerDiv
  appendChild containerDiv rowDiv
  appendChild rowDiv parentDiv
  appendChild parentDiv leftPaddingColDiv
  appendChild parentDiv centerColDiv
  appendChild parentDiv rightPaddingColDiv

  return parentDiv

createDiv :: [(String, String)] -> Client Elem
createDiv as = newElem "div" `with` attributes
  where
    attributes = map (\(name, value) -> attr name =: value) as

-- |Deletes the DOM created for the intial lobby view
deleteLobbyDOM :: IO ()
deleteLobbyDOM = deleteDOM "container-fluid"

-- |Deletes the DOM created for a game in the lobby
deleteGameDOM :: IO ()
deleteGameDOM = deleteDOM "container-fluid"

-- |Helper function that deletes DOM given an identifier from documentBody
deleteDOM :: String -> IO ()
deleteDOM s = withElem s $ \element -> deleteChild documentBody element

-- |Creates a listener for a click event with the Elem with the given String and a function.
clickEventString :: String -> Client () -> Client HandlerInfo
clickEventString identifier fun =
  withElem identifier $ \e ->
    clickEventElem e fun

-- |Creates a listener for a click event with the given 'Elem' and a function.
clickEventElem :: Elem -> Client () -> Client HandlerInfo
clickEventElem e fun =
   onEvent e Click $ \(MouseData _ mb _) ->
      case mb of
        Just MouseLeft -> fun
        Nothing        -> return ()


-- |Queries the server for a list in an interval, applies a function for every item in the list .
listenForChanges :: (Eq a, Binary a) => Remote (Server [a]) -> (Elem -> a -> Client ()) -> Int -> Elem -> Client ()
listenForChanges remoteCall addChildrenToParent updateDelay parent = listenForChanges' []
  where
    listenForChanges' currentData = do
      remoteData <- onServer remoteCall
      if currentData == remoteData
        then
          setTimer (Once updateDelay) $ listenForChanges' currentData
        else
          (do
            clearChildren parent
            mapM_ (addChildrenToParent parent) remoteData
            setTimer (Once updateDelay) $ listenForChanges' remoteData)
      return ()

addChildrenToCenterColumn :: [Elem] -> Client ()
addChildrenToCenterColumn = addChildrenToParent  "centerContent"

addChildrenToLeftColumn :: [Elem] -> Client ()
addChildrenToLeftColumn = addChildrenToParent "leftContent"

addChildrenToRightColumn :: [Elem] -> Client ()
addChildrenToRightColumn = addChildrenToParent "rightContent"

addChildrenToParent :: String -> [Elem] -> Client ()
addChildrenToParent parent children = do
  parentElem <- elemById parent
  mapM_ (appendChild $ fromJust parentElem) children