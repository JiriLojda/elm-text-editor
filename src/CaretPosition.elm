module CaretPosition exposing (..)

import List.Extra as EList

type alias CaretPosition =
  { column : Int
  , line : Int
  }


caretPosToIndex : String -> CaretPosition -> Int
caretPosToIndex textValue caretPos =
    let
      lines = String.lines textValue
      result =
        lines
          |> List.take caretPos.line
          |> List.map String.length
          |> List.map ((+) 1)
          |> List.sum
          |> ((+) caretPos.column)
    in
    max 0 <| min (String.length textValue) result

indexToCaretPos : String -> Int -> CaretPosition
indexToCaretPos textValue index =
    let
      relevantChars = List.take index <| String.toList textValue
      newLinesCount = EList.count ((==) '\n') relevantChars
      lastLine = if newLinesCount == 0 then relevantChars else EList.takeWhileRight ((/=) '\n') relevantChars
    in
    CaretPosition (List.length lastLine) newLinesCount

updateCaretPosByIndexUpdate : (Int -> Int) -> String -> CaretPosition -> CaretPosition
updateCaretPosByIndexUpdate updater str oldPos =
  caretPosToIndex str oldPos
    |> updater
    |> min (String.length str)
    |> max 0
    |> indexToCaretPos str

roundCaretPos : String -> CaretPosition -> CaretPosition
roundCaretPos textValue caretPos =
  let
    lines = String.lines textValue
    lineNum = max 0 <| min (List.length lines - 1) caretPos.line
  in
  case EList.getAt lineNum lines of
    --Nothing -> Debug.log "Failed to find selected line. :O " <| CaretPosition 0 0
    Nothing -> CaretPosition 0 0
    Just line -> CaretPosition (max 0 <| min (String.length line) caretPos.column) lineNum

