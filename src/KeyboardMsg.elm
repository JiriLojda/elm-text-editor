module KeyboardMsg exposing (KeyboardMsg(..), keyboardMsgDecoder)
import Json.Decode as Json
import Tuple exposing (pair)

type KeyboardMsg
        = InsertChar Char
        | MoveCaretLeft
        | MoveCaretRight
        | MoveCaretUp
        | MoveCaretDown
        | MoveCaretLeftWithSelection
        | MoveCaretRightWithSelection
        | MoveCaretUpWithSelection
        | MoveCaretDownWithSelection
        | MoveCaretToLineEnd
        | MoveCaretToLineStart
        | MoveCaretToTheStart
        | MoveCaretToTheEnd
        | MoveCaretToLineEndWithSelection
        | MoveCaretToLineStartWithSelection
        | MoveCaretToTheStartWithSelection
        | MoveCaretToTheEndWithSelection
        | RemovePrevChar
        | RemoveNextChar
        | AddNewLine

keyboardMsgDecoder : Json.Decoder KeyboardMsg
keyboardMsgDecoder =
    Json.map2 pair keyDecoder modifiersDecoder
      |> Json.andThen keyMsgDecoder

type Key
      = CharKey Char
      | ArrowUp
      | ArrowDown
      | ArrowLeft
      | ArrowRight
      | Home
      | End
      | Backspace
      | Delete
      | Enter

type alias Modifiers =
            { ctrlPressed : Bool
            , altPressed : Bool
            , shiftPressed : Bool
            }

keyDecoder : Json.Decoder Key
keyDecoder =
    Json.field "key" Json.string
      |> Json.andThen (\key ->
          let
            char = case String.uncons key of
                    Just (c, "") -> Just c
                    _ -> Nothing
          in
          case (char, key) of
            (Just c, _) -> Json.succeed (CharKey c)
            (_, "ArrowUp") -> Json.succeed ArrowUp
            (_, "ArrowDown") -> Json.succeed ArrowDown
            (_, "ArrowLeft") -> Json.succeed ArrowLeft
            (_, "ArrowRight") -> Json.succeed ArrowRight
            (_, "Home") -> Json.succeed Home
            (_, "End") -> Json.succeed End
            (_, "Backspace") -> Json.succeed Backspace
            (_, "Delete") -> Json.succeed Delete
            (_, "Enter") -> Json.succeed Enter
            (_, str) -> Json.fail <| "Not supported key: '" ++ str ++ "'. (at least for now)"
          )

modifiersDecoder : Json.Decoder Modifiers
modifiersDecoder =
    Json.map3 Modifiers
      (Json.field "ctrlKey" Json.bool)
      (Json.field "altKey" Json.bool)
      (Json.field "shiftKey" Json.bool)

keyMsgDecoder : (Key, Modifiers) -> Json.Decoder KeyboardMsg
keyMsgDecoder (key, modifiers) =
    case (key, (modifiers.ctrlPressed, modifiers.shiftPressed, modifiers.altPressed)) of
      (CharKey c, (False, _, False)) -> Json.succeed (InsertChar c)
      (ArrowLeft, (False, False, False)) -> Json.succeed MoveCaretLeft
      (ArrowRight, (False, False, False)) -> Json.succeed MoveCaretRight
      (ArrowUp, (False, False, False)) -> Json.succeed MoveCaretUp
      (ArrowDown, (False, False, False)) -> Json.succeed MoveCaretDown
      (ArrowLeft, (False, True, False)) -> Json.succeed MoveCaretLeftWithSelection
      (ArrowRight, (False, True, False)) -> Json.succeed MoveCaretRightWithSelection
      (ArrowUp, (False, True, False)) -> Json.succeed MoveCaretUpWithSelection
      (ArrowDown, (False, True, False)) -> Json.succeed MoveCaretDownWithSelection
      (End, (False, False, False)) -> Json.succeed MoveCaretToLineEnd
      (End, (True, False, False)) -> Json.succeed MoveCaretToTheEnd
      (Home, (False, False, False)) -> Json.succeed MoveCaretToLineStart
      (Home, (True, False, False)) -> Json.succeed MoveCaretToTheStart
      (End, (False, True, False)) -> Json.succeed MoveCaretToLineEndWithSelection
      (End, (True, True, False)) -> Json.succeed MoveCaretToTheEndWithSelection
      (Home, (False, True, False)) -> Json.succeed MoveCaretToLineStartWithSelection
      (Home, (True, True, False)) -> Json.succeed MoveCaretToTheStartWithSelection
      (Delete, (False, False, False)) -> Json.succeed RemoveNextChar
      (Backspace, (False, False, False)) -> Json.succeed RemovePrevChar
      (Enter, (False, _, False)) -> Json.succeed AddNewLine
      _ -> Json.fail "Unsupported keyboard shortcut"

