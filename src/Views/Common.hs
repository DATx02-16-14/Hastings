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
deleteLobbyDOM = deleteDOM "lobby" "centerContent"

-- |Deletes the DOM created for a game in the lobby
deleteGameDOM :: Client ()
deleteGameDOM = do
  deleteDOM "lobbyGame" "centerContent"
  deleteDOM "changeGameName" "rightContent"

-- |Helper function that deletes DOM given an identifier from that element and the parent element
deleteDOM :: String -> String -> Client ()
deleteDOM s parent = withElems [s, parent] $ \[element, parentElem] -> deleteChild parentElem element

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
  addChildrenToParent' (fromJust parentElem) children

addChildrenToParent' :: Elem -> [Elem] -> Client ()
addChildrenToParent' parent children = mapM_ (appendChild parent) children

-- |Creates an input field and button. Has 'String' as id and '(Client ())' is the function
-- |That activates when clicking button or pressing enter. Descriptive text is second 'String'
-- |Div is created with id = identifier ++ "Div". Field is created with id = identifier ++ "Field".
-- |Button is created with id = identifier ++ "Btn"
-- |The field and button is then placed in the right sidebar.
createInputFieldWithButton :: String -> String -> (Client ()) -> Client ()
createInputFieldWithButton identifier text function = do
  parentDiv <- createDiv [("id", identifier ++ "Div")]

  textElement <- newTextElem text
  inputField <- newElem "input" `with`
    [
      attr "type" =: "text",
      attr "id" =: (identifier ++ "Field")
    ]
  button <- newElem "button" `with`
    [
      attr "id" =: (identifier ++ "Btn")
    ]
  buttonText <- newTextElem "Change"

  appendChild button buttonText
  appendChild parentDiv textElement
  appendChild parentDiv inputField
  appendChild parentDiv button
  addChildrenToRightColumn [parentDiv]

  onEvent inputField KeyPress $ \13 -> function

  clickEventString (identifier ++ "Btn") function
  return ()
