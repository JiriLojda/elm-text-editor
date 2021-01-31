module SelectionTests exposing (..)

import CaretPosition exposing (CaretPosition)
import CaretPositionTests exposing (caretPosFuzzer)
import Expect
import Fuzz exposing (Fuzzer)
import Selection as Sel exposing (Selection)
import Test exposing (Test, describe, fuzz, fuzz2, test)

suite : Test
suite =
    describe "Selection tests"
      [ describe "isReversed"
        [ fuzz selectionFuzzer "isReversed is false after normalizeSelection"
          (Sel.normalizeSelection >> Sel.isReversed >> Expect.false "normalizeSelection result cannot be reversed (isReversed)")

        , fuzz selectionFuzzer "{ start = firstPosition sel, end = secondPosition sel } cannot be reversed"
            <| \selection ->
              { start = Sel.firstPosition selection, end = Sel.secondPosition selection }
                |> Sel.isReversed
                |> Expect.false "{ start = firstPosition sel, end = secondPosition sel } cannot be reversed"

        , fuzz2 naturalInt naturalInt "start.line > end.line -> isReversed"
            <| \c1 c2 ->
              Sel.isReversed { start = CaretPosition c1 5, end = CaretPosition c2 4 }
                |> Expect.true "start.line > end.line means the selection is reversed"

        , test "start.line == end.line && start.column > end.column -> isReversed"
            <| \_ ->
              Sel.isReversed { start = CaretPosition 5 5, end = CaretPosition 4 5 }
                |> Expect.true "start.line == end.line && start.column > end.column means the selection is reversed"

        , test "start.line == end.line && start.column < end.column -> not isReversed"
            <| \_ ->
              Sel.isReversed { start = CaretPosition 1 5, end = CaretPosition 4 5 }
                |> Expect.false "start.line == end.line && start.column < end.column means the selection is NOT reversed"

        , fuzz2 naturalInt naturalInt "start.line < end.line -> not isReversed"
            <| \c1 c2 ->
              Sel.isReversed { start = CaretPosition c1 3, end = CaretPosition c2 5 }
                |> Expect.false "start.line == end.line && start.column < end.column means the selection is NOT reversed"
        ]

      , describe "getSelectedText"
        [ fuzz selectionFuzzer "reversing selection doesn't change selected text"
          <| \selection ->
            let
              text = "This is some sample text\n There is really nothing special here\n abcd\n\n\n Jeeej jaaaj joooj\n\n\njoijasfjoija"
            in
            Sel.getSelectedText text selection
              |> Expect.equal (Sel.getSelectedText text <| reverseSelection selection)

        , test "returns selected text"
          <| \_ ->
            Sel.getSelectedText "abcd\nefgh" { start = CaretPosition 2 0, end = CaretPosition 2 1 }
              |> Expect.equal "cd\nef"

        , test "returns selected text for reversed selection"
          <| \_ ->
            Sel.getSelectedText "abcd\nefgh" { start = CaretPosition 0 1, end = CaretPosition 0 0 }
              |> Expect.equal "abcd\n"
        ]

      , describe "removeSelectedText"
        [ fuzz Fuzz.string "text remains unchanged with empty selection"
          <| \text ->
            Sel.removeSelectedText Nothing text
              |> Expect.equal text
        , test "removes single-line selections"
          <| \_ ->
            Sel.removeSelectedText (Just { start = CaretPosition 2 1, end = CaretPosition 5 1 }) "abcd\n0123456789\nefgh"
              |> Expect.equal "abcd\n0156789\nefgh"
        , test "remove multi-line selections"
          <| \_ ->
            Sel.removeSelectedText (Just { start = CaretPosition 2 0, end = CaretPosition 5 1 }) "abcd\n0123456789\nefgh"
              |> Expect.equal "ab56789\nefgh"
        ]

      , describe "isPositionSelected"
        [ test "Same line, between start and end columns -> isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 0, end = CaretPosition 5 0 }) (CaretPosition 3 0)
              |> Expect.true "the position should be selected"

        , fuzz Fuzz.int "On a line in-between -> isSelected on any column"
          <| \column ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 0, end = CaretPosition 5 5 }) (CaretPosition column 3)
              |> Expect.true "the position should be selected"

        , test "startLine /= endLine, on startLine, after start -> isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 0, end = CaretPosition 5 5 }) (CaretPosition 3 0)
              |> Expect.true "the position should be selected"

        , test "endLine /= startLine, on endLine, before end -> isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 0, end = CaretPosition 5 5 }) (CaretPosition 3 5)
              |> Expect.true "the position should be selected"

        , test "startLine > line -> not isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 3, end = CaretPosition 5 5 }) (CaretPosition 3 0)
              |> Expect.false "the position should NOT be selected"

        , test "endLine < line -> not isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 3, end = CaretPosition 5 5 }) (CaretPosition 3 7)
              |> Expect.false "the position should NOT be selected"

        , test "startLine /= endLine, onStartLine, before start -> not isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 3, end = CaretPosition 5 5 }) (CaretPosition 0 3)
              |> Expect.false "the position should NOT be selected"

        , test "startLine /= endLine, onEndLine, after end -> not isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 3, end = CaretPosition 5 5 }) (CaretPosition 7 5)
              |> Expect.false "the position should NOT be selected"

        , test "startLine == endLine, not between start and end columns -> not isSelected"
          <| \_ ->
            Sel.isPositionSelected (Just { start = CaretPosition 1 3, end = CaretPosition 5 3 }) (CaretPosition 0 3)
              |> Expect.false "the position should NOT be selected"

        , fuzz2 Fuzz.int Fuzz.int "no selection -> not isSelected"
          <| \column line ->
            Sel.isPositionSelected Nothing (CaretPosition column line)
              |> Expect.false "the position should NOT be selected"

        ]
      , describe "extendSelectionTo"
        [ fuzz2 Fuzz.int Fuzz.int "sets the target position as selection's end position, leaving the start position intact"
          <| \column line ->
            let
              start = CaretPosition 5 5
              end = CaretPosition 7 7
            in
            Sel.extendSelectionTo (CaretPosition column line) { start = start, end = end }
              |> Expect.all
                [ (.start >> Expect.equal start)
                , (.end >> Expect.equal (CaretPosition column line))
                ]
        ]
      ]

selectionFuzzer : Fuzzer Selection
selectionFuzzer =
    Fuzz.map2 Selection (caretPosFuzzer 0 50 0 50) (caretPosFuzzer 0 50 0 50)

naturalInt : Fuzzer Int
naturalInt =
  Fuzz.intRange 0 1000

reverseSelection : Selection -> Selection
reverseSelection sel =
    { start = sel.end, end = sel.start }
