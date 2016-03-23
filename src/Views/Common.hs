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
      attr "rel"          =: "stylesheet",
      attr "type"         =: "text/css",
      attr "href"         =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
      attr "integrity"    =: "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7",
      attr "crossorigin"  =: "anonymous"
    ]

  appendChild documentBody cssLink

createBootstrapTemplate :: String -> Client Elem
createBootstrapTemplate parentName = do

  containerDiv        <- createDiv [("class", "container-fluid"), ("id", "container-fluid")]
  rowDiv              <- createDiv [("class", "row"), ("id", "row")]
  leftPaddingColDiv   <- createDiv [("class", "col-md-3"), ("id", "leftContent")]
  rightPaddingColDiv  <- createDiv [("class", "col-md-3"), ("id", "rightContent")]
  centerColDiv        <- createDiv [("class", "col-md-6"), ("id", "centerContent")]
  parentDiv           <- createDiv [("id", parentName)]

  appendChild documentBody containerDiv
  appendChild containerDiv rowDiv
  appendChild rowDiv parentDiv

  addChildrenToParent parentName [leftPaddingColDiv, centerColDiv, rightPaddingColDiv]
  return parentDiv

createDiv :: [(String, String)] -> Client Elem
createDiv as = newElem "div" `with` attributes
  where
    attributes = map (\(name, value) -> attr name =: value) as

-- |Deletes the DOM created for the intial lobby view
deleteLobbyDOM :: Client ()
deleteLobbyDOM = deleteDOM "container-fluid"

-- |Deletes the DOM created for a game in the lobby
deleteGameDOM :: Client ()
deleteGameDOM = deleteDOM "container-fluid"

-- |Helper function that deletes DOM given an identifier from documentBody
deleteDOM :: String -> Client ()
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
