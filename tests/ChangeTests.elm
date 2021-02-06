module ChangeTests exposing (..)

import CaretPositionTests exposing (caretPosFuzzer, textWithIndexFuzzer)
import Change exposing (Change(..), applyChange)
import Expect
import Fuzz
import List.Extra as EList
import SelectionTests exposing (selectionFuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import CaretPosition as Pos exposing (CaretPosition)

clipboardChanged : Test
clipboardChanged =
    describe "ClipboardChanged"
      [ fuzz2 Fuzz.string Fuzz.bool "text never changes"
        <| \str isLineCopy ->
          applyChange (ClipboardChanged { isLineCopy = isLineCopy, newClipboard = str }) sampleModel
            |> .textValue
            |> Expect.equal sampleModel.textValue

      , fuzz3 Fuzz.string selectionFuzzer (caretPosFuzzer 0 50 0 50) "selection and caretPosition doesn't change if not isLineCopy"
        <| \str selection position ->
          applyChange (ClipboardChanged { isLineCopy = False, newClipboard = str }) { sampleModel | selection = Just selection, caretPosition = position }
            |> Expect.all
              [ (.selection >> Expect.equal (Just selection))
              , (.caretPosition >> Expect.equal position)
              ]

      , fuzz2 Fuzz.string Fuzz.string "clipboard is always assigned contents of newClipboard"
        <| \old new ->
          applyChange (ClipboardChanged { isLineCopy = False, newClipboard = new }) { sampleModel | clipboard = old }
            |> .clipboard
            |> Expect.equal new

      , fuzz (caretPosFuzzer 0 5 0 5) "selection is assigned to the whole line where caret currently is if isLineCopy"
        <| \caretPosition ->
          let
            text = "abcd\n123456789\nefgh\nijkl\nmnop\nhouse\nhome\ntree\ndrive\nroof"
            line = EList.getAt caretPosition.line (String.lines text)
            lineLength = String.length <| Maybe.withDefault "" line
          in
          applyChange (ClipboardChanged { isLineCopy = True, newClipboard = "something" })
            { sampleModel
            | caretPosition = caretPosition
            , textValue = text
            }
            |> Expect.all
              [ (.caretPosition >> Expect.equal (CaretPosition 0 caretPosition.line))
              , (.selection >> Expect.equal (Just { start = CaretPosition lineLength caretPosition.line, end = CaretPosition 0 caretPosition.line  }))
              ]
      ]

textInserted : Test
textInserted =
    describe "TextInserted"
      [ fuzz2 textWithIndexFuzzer Fuzz.string "text is inserted at caret position"
        <| \(currentText, caretIndex) toInsert ->
          let
            caretPosition = Pos.indexToCaretPos currentText caretIndex
          in
          applyChange (TextInserted { toInsert = toInsert }) { sampleModel | textValue = currentText, caretPosition = caretPosition }
            |> .textValue
            |> String.slice caretIndex (caretIndex + String.length toInsert)
            |> Expect.equal toInsert

      , fuzz2 textWithIndexFuzzer Fuzz.string "caret position is moved after the inserted text"
        <| \(currentText, caretIndex) toInsert ->
          let
            caretPosition = Pos.indexToCaretPos currentText caretIndex
            result = applyChange (TextInserted { toInsert = toInsert }) { sampleModel | textValue = currentText, caretPosition = caretPosition }
          in
          result
            |> .caretPosition
            |> Expect.equal (Pos.indexToCaretPos result.textValue (caretIndex + String.length toInsert))

      , test "selection exists -> new text is placed in place of a selection"
        <| \_ ->
          applyChange (TextInserted { toInsert = "abcd" })
            { sampleModel
            | textValue = "1234\n5678\n90"
            , selection = Just { start = CaretPosition 3 0, end = CaretPosition 2 1 }
            , caretPosition = CaretPosition 2 1
            }
            |> Expect.all
              [ (.textValue >> Expect.equal "123abcd78\n90")
              , (.selection >> Expect.equal Nothing)
              ]

      , test "reversed selection exists -> new text is placed in place of a selection"
        <| \_ ->
          applyChange (TextInserted { toInsert = "abcd" })
            { sampleModel
            | textValue = "1234\n5678\n90"
            , selection = Just { end = CaretPosition 3 0, start = CaretPosition 2 1 }
            , caretPosition = CaretPosition 3 0
            }
            |> Expect.all
              [ (.textValue >> Expect.equal "123abcd78\n90")
              , (.selection >> Expect.equal Nothing)
              ]

      , fuzz3 (Fuzz.maybe selectionFuzzer) longTextCaretPosFuzzer Fuzz.string "selection is always removed"
        <| \selection caretPos toInsert ->
            applyChange (TextInserted { toInsert = toInsert })
              { sampleModel
              | textValue = longText
              , selection = selection
              , caretPosition = caretPos
              }
              |> .selection
              |> Expect.equal Nothing
      ]

charRemoved : Test
charRemoved =
    describe "CharRemoved"
      [ fuzz Fuzz.int "has no effect with empty textValue"
        <| \index ->
          let
            model = { sampleModel | textValue = "" }
          in
          applyChange (CharRemoved { index = index }) model
            |> Expect.equal model

      , fuzz2 Fuzz.string (Fuzz.intRange 0 20) "selection exists -> removes selection and nothing else"
        <| \text index ->
          let
            originalText = "12345\n67890\nabcde\nfghijklmop"
            newText = String.slice 0 index originalText ++ text ++ String.dropLeft index originalText
            model =
              { textValue = newText
              , caretPosition = Pos.indexToCaretPos newText index
              , selection = Just { start = Pos.indexToCaretPos newText (index + String.length text), end = Pos.indexToCaretPos newText index }
              , clipboard = ""
              }
          in
          applyChange (CharRemoved { index = index }) model
            |> Expect.all
              [ (.textValue >> Expect.equal originalText)
              , (.selection >> Expect.equal Nothing)
              , (.caretPosition >> Expect.equal (Pos.indexToCaretPos originalText index))
              ]

      , fuzz textWithIndexFuzzer "removes char at specified index"
        <| \(text, index) ->
          let
            withCharToBeRemoved = String.slice 0 index text ++ "$" ++ String.dropLeft index text
          in
          applyChange (CharRemoved { index = index }) { sampleModel | textValue = withCharToBeRemoved }
            |> .textValue
            |> Expect.equal text

      , test "moves caretPosition to the removed index if the index is before the caretPosition"
        <| \_ ->
          applyChange (CharRemoved { index = 2 }) { sampleModel | caretPosition = CaretPosition 3 0 }
            |> .caretPosition
            |> Expect.equal (CaretPosition 2 0)

      , test "doesn't change caretPosition if removed char is on the same index"
        <| \_ ->
          applyChange (CharRemoved { index = 2 }) { sampleModel | caretPosition = CaretPosition 2 0 }
            |> .caretPosition
            |> Expect.equal (CaretPosition 2 0)
      ]

caretMoved : Test
caretMoved =
    describe "CaretMoved"
      [ fuzz2 longTextCaretPosFuzzer longTextCaretPosFuzzer "caretPosition is changed to the given position"
        <| \oldPosition newPosition ->
          applyChange (CaretMoved { toPosition = newPosition, withSelection = False })
            { sampleModel
            | textValue = longText
            , caretPosition = oldPosition
            }
            |> .caretPosition
            |> Expect.equal newPosition

      , fuzz3 longTextCaretPosFuzzer longTextCaretPosFuzzer (Fuzz.maybe selectionFuzzer) "withoutSelection -> selection is always Nothing"
        <| \oldPosition newPosition selection ->
          let
            model =
              { sampleModel
              | textValue = longText
              , caretPosition = oldPosition
              , selection = selection
              }
          in
          applyChange (CaretMoved { toPosition = newPosition, withSelection = False }) model
            |> .selection
            |> Expect.equal Nothing

      , fuzz2 longTextCaretPosFuzzer longTextCaretPosFuzzer "withSelection, no existing selection -> selection starts at original caretPosition and ends at the target position"
        <| \oldPosition newPosition ->
          let
            model =
              { sampleModel
              | textValue = longText
              , caretPosition = oldPosition
              }
          in
          applyChange (CaretMoved { toPosition = newPosition, withSelection = True }) model
            |> .selection
            |> Expect.equal (Just { start = oldPosition, end = newPosition })

      , fuzz3 longTextCaretPosFuzzer longTextCaretPosFuzzer selectionFuzzer "withSelection, existing selection -> selection is extended to the new position (end position)"
        <| \oldPosition newPosition selection ->
          let
            model =
              { sampleModel
              | textValue = longText
              , caretPosition = oldPosition
              , selection = Just selection
              }
          in
          applyChange (CaretMoved { toPosition = newPosition, withSelection = True }) model
            |> .selection
            |> Expect.equal (Just { start = selection.start, end = newPosition })
      ]

sampleModel =
  { textValue = "abcd\n123456789\nefgh"
  , caretPosition = CaretPosition 0 0
  , selection = Nothing
  , clipboard = ""
  }

longTextCaretPosFuzzer =
  caretPosFuzzer 0 50 0 50

longText =
  "1234567890"
    |> List.repeat 5
    |> String.join ""
    |> List.repeat 50
    |> String.join "\n"
