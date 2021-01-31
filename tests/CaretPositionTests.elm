module CaretPositionTests exposing (suite, caretPosFuzzer)

import CaretPosition as Pos exposing (CaretPosition)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, string)
import Random exposing (Generator)
import Shrink exposing (noShrink)
import Test exposing (..)
import List.Extra as EList


suite : Test
suite =
    describe "CaretPosition tests"
      [ describe "converts properly between index and CaretPosition"
          [ fuzz textWithIndexFuzzer "conversion index -> CaretPosition -> index maintains the same index"
              <| \(text, index) ->
                    Pos.indexToCaretPos text index
                      |> Pos.caretPosToIndex text
                      |> Expect.equal index
          ]
      , describe "roundCaretPos:"
          [ fuzz2 Fuzz.string (caretPosFuzzer -5 800 -5 800) "roundCaretPos returns CaretPosition that is always within the given string"
            <| \str pos ->
                  let
                    lines = String.lines str
                    result = Pos.roundCaretPos str pos
                  in
                  result
                    |> Expect.all
                      [ (.line >> Expect.atLeast 0)
                      , (.column >> Expect.atLeast 0)
                      , (.line >> Expect.atMost (List.length lines - 1))
                      , case EList.getAt result.line lines of
                          Nothing -> always <| Expect.fail "Returned line does not exist in the text"
                          Just s -> (.column >> Expect.atMost (String.length s))
                      ]
          , test "line of linesCount to linesCount - 1"
              <| \_ ->
                  Pos.roundCaretPos "\n\nabcd" (CaretPosition 0 3)
                    |> (.line >> Expect.equal 2)
          , test "line exists, column > lineChars + 1 -> column to lineChars"
              <| \_ ->
                  Pos.roundCaretPos "\n\nabcd\nohno\n" (CaretPosition 50 2)
                    |> Expect.equal (CaretPosition 4 2)
          , fuzz (caretPosFuzzer 0 4 0 3) "position inside the text -> doesn't change"
              <| \pos ->
                  Pos.roundCaretPos "abcd\nabcd\nabcd\nabcd" pos
                    |> Expect.equal pos
          ]
      , describe "updateCaretPosByIndexUpdate:"
          [ fuzz2 (Fuzz.intRange 0 20) (caretPosFuzzer 0 4 0 3) "result is always inside the text"
              <| \index pos ->
                  Pos.updateCaretPosByIndexUpdate ((+) index) "abcd\nabcd\nabcd\nabcd" pos
                    |> Expect.all
                        [ (.line >> Expect.atLeast 0)
                        , (.column >> Expect.atLeast 0)
                        , (.line >> Expect.atMost 3)
                        , (.column >> Expect.atMost 4)
                        ]
          , test "with move over \\n updates line accordingly"
              <| \_ ->
                let
                  text = "abcd\nefgh"
                  forwardRes = Pos.updateCaretPosByIndexUpdate ((+) 3) text (CaretPosition 3 0)
                  backRes = Pos.updateCaretPosByIndexUpdate ((-) 3) text (CaretPosition 1 1)
                in
                { forward = forwardRes, back = backRes }
                  |> Expect.all
                    [ (.back >> .line >> Expect.equal 0)
                    , (.forward >> .line >> Expect.equal 1)
                    ]
          ]
      ]

caretPosFuzzer : Int -> Int -> Int -> Int -> Fuzzer CaretPosition
caretPosFuzzer colStart colEnd lineStart lineEnd =
    Fuzz.map2
      CaretPosition
      (Fuzz.intRange colStart colEnd)
      (Fuzz.intRange lineStart lineEnd)

textWithIndexFuzzer : Fuzzer (String, Int)
textWithIndexFuzzer =
    let
      generator =
        string
          |> Random.andThen (\str -> Random.pair (Random.constant str) (Random.int 0 <| String.length str))
    in
    Fuzz.custom generator noShrink


-- Copied from Fuzz as I found no way to generate tuple of string and index into the string


string : Generator String
string =
    let
        asciiGenerator : Generator String
        asciiGenerator =
            frequency
                ( 3, Random.int 1 10 )
                [ ( 0.2, Random.constant 0 )
                , ( 1, Random.int 11 50 )
                , ( 1, Random.int 50 1000 )
                ]
                |> Random.andThen (lengthString asciiCharGenerator)

        whitespaceGenerator : Generator String
        whitespaceGenerator =
            Random.int 1 10
                |> Random.andThen (lengthString whitespaceCharGenerator)
    in
    (frequency
        ( 9, asciiGenerator )
        [ ( 1, whitespaceGenerator )
        ]
    )

asciiCharGenerator : Generator Char
asciiCharGenerator =
    Random.map Char.fromCode (Random.int 32 126)


whitespaceCharGenerator : Generator Char
whitespaceCharGenerator =
    sample [ ' ', '\t', '\n' ] |> Random.map (Maybe.withDefault ' ')


sample : List a -> Generator (Maybe a)
sample =
    let
        find k ys =
            case ys of
                [] ->
                    Nothing

                z :: zs ->
                    if k == 0 then
                        Just z

                    else
                        find (k - 1) zs
    in
    \xs -> Random.map (\i -> find i xs) (Random.int 0 (List.length xs - 1))

frequency : ( Float, Generator a ) -> List ( Float, Generator a ) -> Generator a
frequency firstPair restPairs =
    let
        total =
            List.sum <| List.map (abs << Tuple.first) (firstPair :: restPairs)

        pick ( k, g ) restChoices n =
            if n <= k then
                g

            else
                case restChoices of
                    [] ->
                        g

                    next :: rest ->
                        pick next rest (n - k)
    in
    Random.float 0 total |> Random.andThen (pick firstPair restPairs)

lengthString : Generator Char -> Int -> Generator String
lengthString charGenerator stringLength =
    Random.list stringLength charGenerator
        |> Random.map String.fromList
