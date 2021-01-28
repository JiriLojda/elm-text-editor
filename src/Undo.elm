module Undo exposing (UndoStack, applyChangeToUndoStack, undoLastBatch)

import CaretPosition as Pos exposing (CaretPosition)
import Change as Change exposing (Change)
import Selection as Sel exposing (Selection)

type alias TextRemovedData =
  { startingIndex : Int
  , text : String
  , wasSelected : Bool
  , originalCaretPosition : CaretPosition
  }

type alias TextAddedData =
  { index : Int
  , text : String
  }

type alias CaretMovedData =
  { from : CaretPosition
  , to : CaretPosition
  }

type UndoableChange
  = TextRemoved TextRemovedData
  | TextAdded TextAddedData
  | CaretMoved CaretMovedData

type alias UndoableEditorInfo model =
  { model
  | textValue : String
  , caretPosition : CaretPosition
  , selection : Maybe Selection
  }

undoChange : UndoableChange -> UndoableEditorInfo model -> UndoableEditorInfo model
undoChange change model =
    case change of
      TextRemoved data ->
        let
          startingPos = Pos.indexToCaretPos model.textValue data.startingIndex
          wasAfterCaret = startingPos == data.originalCaretPosition
          newText = insertAt data.startingIndex data.text model.textValue
          endingPos = Pos.indexToCaretPos newText (data.startingIndex + String.length data.text)
        in
        { model
        | textValue = newText
        , caretPosition = data.originalCaretPosition
        , selection =
            if data.wasSelected
              then Just { start = if wasAfterCaret then endingPos else startingPos, end = if wasAfterCaret then startingPos else endingPos }
              else model.selection
        }
      TextAdded data ->
        { model
        | textValue = removeAt data.index (String.length data.text) model.textValue
        , caretPosition = Pos.indexToCaretPos model.textValue data.index
        , selection = Nothing
        }
      CaretMoved data ->
        { model
        | caretPosition = data.from
        , selection = Nothing
        }

type alias UndoableBatch = List UndoableChange
type alias UndoStack = List UndoableBatch

type alias WithUndoStack model =
  { model
  | undoStack : UndoStack
  }

undoLastBatch : WithUndoStack (UndoableEditorInfo model) -> WithUndoStack (UndoableEditorInfo model)
undoLastBatch model =
    let
      updatedModel = List.foldl undoChange model <| Maybe.withDefault [] (List.head model.undoStack)
    in
    { updatedModel
    | undoStack = List.drop 1 model.undoStack
    }

applyChangeToUndoStack : Change -> WithUndoStack (UndoableEditorInfo model) -> WithUndoStack (UndoableEditorInfo model)
applyChangeToUndoStack change model =
    let
      lastBatch = Maybe.withDefault [] <| List.head model.undoStack
      lastChange = List.head lastBatch
    in
    case change of
      Change.CaretMoved data ->
        let
          newStack =
            case lastChange of
              Nothing ->
                [CaretMoved { to = data.toPosition, from = model.caretPosition }] :: model.undoStack
              Just lstChange ->
                case lstChange of
                  CaretMoved { from } ->
                    model.undoStack
                      |> changeOrAddFirstElement (changeOrAddFirstElement (CaretMoved { from = from, to = data.toPosition }) lastBatch)
                  _ -> [CaretMoved { to = data.toPosition, from = model.caretPosition }] :: model.undoStack
        in
        { model
        | undoStack = if data.toPosition == model.caretPosition then model.undoStack else newStack
        }
      Change.TextInserted data ->
        let
          maybeSelectionRemoval = createSelectionRemovedChange model
          selectionRemoval =
            case maybeSelectionRemoval of
              Nothing ->
                []
              Just sel ->
                [sel]
          index =
            case model.selection of
              Nothing ->
                Pos.caretPosToIndex model.textValue model.caretPosition
              Just sel ->
                Pos.caretPosToIndex model.textValue (Sel.firstPosition sel)
          adding = TextAdded { index = index, text = data.toInsert }
          hasMoreThanOneChar = String.length data.toInsert > 1
          shouldCreateNewBatch =
            case lastChange of
              Nothing -> hasMoreThanOneChar
              Just (TextAdded _) -> hasMoreThanOneChar
              _ -> True
        in
        { model
        | undoStack =
            if shouldCreateNewBatch
              then (adding :: selectionRemoval) :: model.undoStack
              else appendBatchToLatestBatch (adding :: selectionRemoval) model.undoStack
        }
      Change.CharRemoved data ->
        let
          removalChange =
            case createSelectionRemovedChange model of
              Nothing ->
                TextRemoved
                  { startingIndex = data.index
                  , text = String.slice data.index (data.index + 1) model.textValue
                  , wasSelected = False
                  , originalCaretPosition = model.caretPosition
                  }
              Just c ->
                c
          shouldCreateNewBatch =
            case lastChange of
              Nothing -> False
              Just (TextRemoved _) -> False
              _ -> True
        in
        { model
        | undoStack =
            if shouldCreateNewBatch
              then [removalChange] :: model.undoStack
              else appendToLatestBatch removalChange model.undoStack
        }
      Change.ClipboardChanged _ ->
        model


appendToLatestBatch : UndoableChange -> List UndoableBatch -> List UndoableBatch
appendToLatestBatch change stack =
    case stack of
      [] -> [[change]]
      (last :: rest) -> (change :: last) :: rest

appendBatchToLatestBatch : UndoableBatch -> List UndoableBatch -> List UndoableBatch
appendBatchToLatestBatch batch stack =
    List.foldr appendToLatestBatch stack batch

changeOrAddFirstElement : a -> List a -> List a
changeOrAddFirstElement element list =
    case list of
      [] -> [element]
      (_ :: rest) -> element :: rest

createSelectionRemovedChange : UndoableEditorInfo model -> Maybe UndoableChange
createSelectionRemovedChange model =
    case model.selection of
      Nothing ->
        Nothing
      Just sel ->
        let
          startIndex = Pos.caretPosToIndex model.textValue <| Sel.firstPosition sel
          endIndex = Pos.caretPosToIndex model.textValue <| Sel.secondPosition sel
        in
        Just <| TextRemoved
          { startingIndex = startIndex
          , text = String.slice startIndex endIndex model.textValue
          , originalCaretPosition = model.caretPosition
          , wasSelected = True
          }

insertAt : Int -> String -> String -> String
insertAt index toInsert target =
    String.slice 0 index target ++ toInsert ++ String.slice index (String.length target) target

removeAt : Int -> Int -> String -> String
removeAt index count string =
    String.slice 0 index string ++ String.slice (index + count) (String.length string) string
