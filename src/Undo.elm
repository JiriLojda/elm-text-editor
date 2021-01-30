module Undo exposing (UndoStack, applyChangeToUndoStack, undoLastBatch, redoLastBatch, RedoStack)

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
type alias RedoStack = UndoStack

type alias WithUndoRedoStacks model =
  { model
  | undoStack : UndoStack
  , redoStack : RedoStack
  }

undoLastBatch : WithUndoRedoStacks (UndoableEditorInfo model) -> WithUndoRedoStacks (UndoableEditorInfo model)
undoLastBatch model =
    let
      updatedModel = List.foldl undoChange model <| Maybe.withDefault [] (List.head model.undoStack)
    in
    { updatedModel
    | undoStack = List.drop 1 model.undoStack
    , redoStack =
        case List.head model.undoStack of
          Nothing ->
            model.redoStack
          Just batch ->
            (List.reverse batch) :: model.redoStack
    }

redoLastBatch : WithUndoRedoStacks (UndoableEditorInfo model) -> WithUndoRedoStacks (UndoableEditorInfo model)
redoLastBatch model =
    let
      updatedModel = List.foldl redoChange model <| Maybe.withDefault [] (List.head model.redoStack)
    in
    { updatedModel
    | redoStack = List.drop 1 model.redoStack
    , undoStack =
        case List.head model.redoStack of
          Nothing ->
            model.undoStack
          Just batch ->
            (List.reverse batch) :: model.undoStack
    }

redoChange : UndoableChange -> UndoableEditorInfo model -> UndoableEditorInfo model
redoChange change model =
    case change of
      TextAdded data ->
        let
          newText = insertAt data.index data.text model.textValue
        in
        { model
        | textValue = newText
        , caretPosition = Pos.indexToCaretPos newText (data.index + String.length data.text)
        , selection = Nothing
        }
      TextRemoved data ->
        { model
        | textValue = removeAt data.startingIndex (String.length data.text) model.textValue
        , caretPosition = data.originalCaretPosition
        , selection = Nothing
        }
      CaretMoved data ->
        { model
        | caretPosition = data.to
        , selection = Nothing
        }


applyChangeToUndoStack : Change -> WithUndoRedoStacks (UndoableEditorInfo model) -> WithUndoRedoStacks (UndoableEditorInfo model)
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
          isNewLineChar = data.toInsert == "\n"
          shouldCreateNewBatch =
            case lastChange of
              Nothing -> hasMoreThanOneChar || isNewLineChar
              Just (TextAdded { text }) -> hasMoreThanOneChar || isNewLineChar || text == "\n"
              _ -> True
        in
        { model
        | undoStack =
            if shouldCreateNewBatch
              then (adding :: selectionRemoval) :: model.undoStack
              else appendBatchToLatestBatch (adding :: selectionRemoval) model.undoStack
        , redoStack = []
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
        , redoStack = []
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
