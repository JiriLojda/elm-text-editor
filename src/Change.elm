module Change exposing (applyChange, Change(..), MoveCaretData, CharRemovedData, TextInsertedData, ClipboardChangedData)


import Selection as Sel exposing (Selection)
import CaretPosition as Pos exposing (CaretPosition)

type alias ClipboardChangedData =
  { newClipboard : String
  , isLineCopy : Bool
  }

type alias TextInsertedData =
  { toInsert : String
  }

type alias CharRemovedData =
  { index : Int
  }

type alias MoveCaretData =
  { toPosition : CaretPosition
  , withSelection : Bool
  }

type Change
  = ClipboardChanged ClipboardChangedData
  | TextInserted TextInsertedData
  | CharRemoved CharRemovedData
  | CaretMoved MoveCaretData

type alias EditorInfo model =
  { model
  | textValue : String
  , caretPosition : CaretPosition
  , selection : Maybe Selection
  , clipboard : String
  }

applyChange : Change -> EditorInfo model  -> EditorInfo model
applyChange change model =
    case change of
      ClipboardChanged data ->
        { model
        | clipboard = data.newClipboard
        , selection =
                  if data.isLineCopy
                    then Just { start = CaretPosition 0 model.caretPosition.line, end = CaretPosition (String.length data.newClipboard) model.caretPosition.line }
                    else model.selection
        , caretPosition =
            if data.isLineCopy then CaretPosition 0 model.caretPosition.line else model.caretPosition
        }
      TextInserted data ->
        let
          withoutSelection = Sel.removeSelectedText model.selection model.textValue
          normalizedSelection = Maybe.map Sel.normalizeSelection model.selection
          index =
            case normalizedSelection of
              Nothing -> Pos.caretPosToIndex withoutSelection model.caretPosition
              Just sel -> Pos.caretPosToIndex withoutSelection sel.start
          finalText = String.slice 0 index withoutSelection ++ data.toInsert ++ String.slice index (String.length withoutSelection) withoutSelection
        in
        { model
        | textValue = finalText
        , caretPosition = Pos.indexToCaretPos finalText (index + String.length data.toInsert)
        , selection = Nothing
        }
      CharRemoved data ->
        { model
        | textValue =
             case model.selection of
               Nothing -> removeCharAt data.index model.textValue
               Just _ -> Sel.removeSelectedText model.selection model.textValue
        , caretPosition =
            case model.selection of
              Nothing -> Pos.updateCaretPosByIndexUpdate (always data.index) model.textValue model.caretPosition
              Just sel -> Sel.firstPosition sel
        , selection = Nothing
        }
      CaretMoved data ->
        { model
        | caretPosition = data.toPosition
        , selection =
            if data.withSelection
              then Just <| Sel.extendSelectionTo data.toPosition <| Maybe.withDefault (Selection model.caretPosition model.caretPosition) model.selection
              else Nothing
        }

removeCharAt : Int -> String -> String
removeCharAt pos str =
    String.left pos str ++ String.dropLeft (pos + 1) str
