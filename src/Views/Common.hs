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
  deleteDOM "lobby-game" "centerContent"
  deleteDOM "game-name-div" "rightContent"
  deleteDOM "max-number-div" "rightContent"
  deleteDOM "set-password-div" "rightContent"

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
addChildrenToParent' parent = mapM_ (appendChild parent)

-- |Creates an input field and button. Has 'String' as id and '(Client ())' is the function
-- |That activates when clicking button or pressing enter. Descriptive text is second 'String'
-- |Div is created with id = identifier ++ "Div". Field is created with id = identifier ++ "Field".
-- |Button is created with id = identifier ++ "Btn"
-- |The field and button is then placed in the right sidebar.
createInputFieldWithButton :: String -> String -> Client () -> Client ()
createInputFieldWithButton identifier text function = do
  parentDiv <- createDiv [("id", identifier ++ "-div"),("class","input-group")]

  inputField <- newElem "input" `with`
    [
      attr "type"        =: "text",
      attr "id"          =: (identifier ++ "-field"),
      attr "placeholder" =: text,
      attr "class"       =: "form-control"
    ]

  buttonSpan <- newElem "span" `with`
    [
      attr "class" =: "input-group-btn"
    ]
  button <- newElem "button" `with`
    [
      attr "id"    =: (identifier ++ "-btn"),
      attr "type"  =: "button",
      attr "class" =: "btn btn-default"
    ]
  buttonText <- newTextElem "Change"

  appendChild button buttonText
  appendChild buttonSpan button
  appendChild parentDiv inputField
  appendChild parentDiv buttonSpan
  addChildrenToRightColumn [parentDiv]

  onEvent inputField KeyPress $ \13 -> function

  clickEventString (identifier ++ "-btn") function
  return ()

-- |Displays the error message in 'String'. Fades in the message and then out.
showError :: String -> Client ()
showError string = do
  stringDiv <- newElem "h3" `with`
    [
      style "text-align" =: "center",
      style "color"      =: "red"
    ]
  stringElem <- newTextElem string
  addChildrenToParent' stringDiv [stringElem]
  addChildrenToCenterColumn [stringDiv]
  fadeInOutElem stringDiv

-- |Fades in and then out a message after displaying it for 5 seconds
fadeInOutElem :: Elem -> Client ()
fadeInOutElem e = do
  fadeInElem e
  setTimer (Once 5000) $ fadeOutElem e
  return ()

-- |Fades in an element (can not be a text element)
fadeInElem :: Elem -> Client ()
fadeInElem e = do
  setStyle e "display" "block"
  fadeInElem' 0.1
  where
    fadeInElem' :: Float -> Client ()
    fadeInElem' op | op > 1   = return ()
                   | otherwise = do
      setStyle e "opacity" $ show op
      setStyle e "filter" $ "alpha(opacity=" ++ show (op * 100) ++ ")"
      setTimer (Once 10) $ fadeInElem' (op + op * 0.1)
      return ()

-- |Fades out an element (can not be a text element)
fadeOutElem :: Elem -> Client ()
fadeOutElem e = fadeOutElem' 1
  where
    fadeOutElem' :: Float -> Client ()
    fadeOutElem' op | op <= 0.1 = do
      setStyle e "display" "none"
      return ()
                    | otherwise = do
      setStyle e "opacity" $ show op
      setStyle e "filter" $ "alpha(opacity=" ++ show (op * 100) ++ ")"
      setTimer (Once 10) $ fadeOutElem' (op - op * 0.1)
      return ()
