module Change exposing (Change(..), CharRemovedData, ClipboardChangedData, MoveCaretData, TextInsertedData, applyChange)

import CaretPosition as Pos exposing (CaretPosition)
import List.Extra as EList
import Selection as Sel exposing (Selection)


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


type alias ChangeableEditorInfo model =
    { model
        | textValue : String
        , caretPosition : CaretPosition
        , selection : Maybe Selection
        , clipboard : String
    }


applyChange : Change -> ChangeableEditorInfo model -> ChangeableEditorInfo model
applyChange change model =
    case change of
        ClipboardChanged data ->
            let
                lines =
                    String.lines model.textValue

                line =
                    EList.getAt model.caretPosition.line lines

                isLastLine =
                    model.caretPosition.line == List.length lines - 1

                lineLength =
                    (String.length <| Maybe.withDefault "" line)
                        + (if isLastLine then
                            0

                           else
                            1
                          )
            in
            { model
                | clipboard =
                    data.newClipboard
                , selection =
                    if data.isLineCopy then
                        Just
                            { start = CaretPosition lineLength model.caretPosition.line
                            , end = CaretPosition 0 model.caretPosition.line
                            }

                    else
                        model.selection
                , caretPosition =
                    if data.isLineCopy then
                        CaretPosition 0 model.caretPosition.line

                    else
                        model.caretPosition
            }

        TextInserted data ->
            let
                withoutSelection =
                    Sel.removeSelectedText model.selection model.textValue

                normalizedSelection =
                    Maybe.map Sel.normalizeSelection model.selection

                index =
                    case normalizedSelection of
                        Nothing ->
                            Pos.caretPosToIndex withoutSelection model.caretPosition

                        Just sel ->
                            Pos.caretPosToIndex withoutSelection sel.start

                finalText =
                    String.slice 0 index withoutSelection ++ data.toInsert ++ String.slice index (String.length withoutSelection) withoutSelection
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
                        Nothing ->
                            removeCharAt data.index model.textValue

                        Just _ ->
                            Sel.removeSelectedText model.selection model.textValue
                , caretPosition =
                    case model.selection of
                        Nothing ->
                            Pos.updateCaretPosByIndexUpdate (always data.index) model.textValue model.caretPosition

                        Just sel ->
                            Sel.firstPosition sel
                , selection = Nothing
            }

        CaretMoved data ->
            { model
                | caretPosition = data.toPosition
                , selection =
                    if data.withSelection then
                        Just <| Sel.extendSelectionTo data.toPosition <| Maybe.withDefault (Selection model.caretPosition model.caretPosition) model.selection

                    else
                        Nothing
            }


removeCharAt : Int -> String -> String
removeCharAt pos str =
    String.left pos str ++ String.dropLeft (pos + 1) str
