module Selection exposing (..)

import CaretPosition as Pos exposing (CaretPosition)


type alias Selection =
    { start : CaretPosition
    , end : CaretPosition
    }


isReversed : Selection -> Bool
isReversed selection =
    selection.end.line
        < selection.start.line
        || (selection.end.line == selection.start.line && selection.end.column < selection.start.column)


isPositionSelected : Maybe Selection -> CaretPosition -> Bool
isPositionSelected maybeSelection { line, column } =
    let
        selection =
            Maybe.withDefault { start = CaretPosition 0 0, end = CaretPosition 0 0 } maybeSelection

        { end, start } =
            if isReversed selection then
                { start = selection.end, end = selection.start }

            else
                selection

        isOnelineSelection =
            end.line == start.line
    in
    (isOnelineSelection && line == end.line && end.column > column && start.column <= column)
        || (end.line > line && start.line < line)
        || (not isOnelineSelection && end.line == line && end.column > column)
        || (not isOnelineSelection && start.line == line && start.column <= column)


firstPosition : Selection -> CaretPosition
firstPosition selection =
    if isReversed selection then
        selection.end

    else
        selection.start


secondPosition : Selection -> CaretPosition
secondPosition selection =
    if isReversed selection then
        selection.start

    else
        selection.end


getSelectedText : String -> Selection -> String
getSelectedText text selection =
    let
        normalizedSelection =
            if isReversed selection then
                { start = selection.end, end = selection.start }

            else
                selection

        startIndex =
            Pos.caretPosToIndex text normalizedSelection.start

        endIndex =
            Pos.caretPosToIndex text normalizedSelection.end
    in
    String.slice startIndex endIndex text


extendSelectionTo : CaretPosition -> Selection -> Selection
extendSelectionTo pos selection =
    { selection | end = pos }


normalizeSelection : Selection -> Selection
normalizeSelection selection =
    if isReversed selection then
        { start = selection.end, end = selection.start }

    else
        selection


removeSelectedText : Maybe Selection -> String -> String
removeSelectedText selection text =
    case selection of
        Nothing ->
            text

        Just sel ->
            let
                correctedSel =
                    if isReversed sel then
                        { start = sel.end, end = sel.start }

                    else
                        sel

                startIndex =
                    Pos.caretPosToIndex text correctedSel.start

                endIndex =
                    Pos.caretPosToIndex text correctedSel.end
            in
            String.left startIndex text ++ String.dropLeft endIndex text


isEmptySelection : Maybe Selection -> Bool
isEmptySelection maybeSelection =
    case maybeSelection of
        Nothing ->
            True

        Just { start, end } ->
            start == end
