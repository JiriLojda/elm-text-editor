module Main exposing (init, update, view, Msg, Model, StyledFragment)
import Basics.Extra exposing (flip, maxSafeInteger)
import Browser
import Browser.Dom as Dom exposing (Element)
import Change exposing (Change(..), applyChange)
import Css exposing (..)
import Css.Animations as Animation exposing (keyframes)
import Css.Global as Global exposing (global)
import Dict exposing (Dict)
import Html.Styled.Events exposing (on, onInput, preventDefaultOn)
import Html.Styled exposing (Html, br, div, span, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (class, css, id, style, tabindex)
import Html.Styled.Lazy exposing (lazy3)
import Json.Decode as Json
import KeyboardMsg exposing (KeyboardMsg(..), keyboardMsgDecoder)
import List.Extra as EList exposing (dropWhile, last, takeWhile)
import Parser exposing ((|.), (|=), Parser)
import String
import Task as Task
import CaretPosition as Pos exposing (CaretPosition)
import Selection as Sel exposing (Selection)
import Undo exposing (RedoStack, UndoStack, applyChangeToUndoStack, redoLastBatch, undoLastBatch)

init : Parser (List StyledFragment) -> flags -> (Model, Cmd Msg)
init highlighter =
  always (
   { textValue = "empty"
   , isSelectionInProgress = False
   , caretPosition = CaretPosition 0 0
   , selection = Nothing
   , highlighter = highlighter
   , clipboard = ""
   , undoStack = []
   , redoStack = []
   , charSize = 0
   , viewport = { top = 0, left = 0, height = 100, width = 100 }
   }, Cmd.batch [measureCharSize, measureViewport])

measureCharSize : Cmd Msg
measureCharSize =
  Dom.getElement "charTester"
    |> Task.map (\x -> x.element.width / 60)
    |> Task.attempt
      (\result ->
        case result of
          Ok size -> CharMeasured size
          Err err -> DebugFail <| Debug.toString err
      )

measureViewport : Cmd Msg
measureViewport =
  Dom.getElement editorId
    |> Task.map (\v -> { top = 0, left = 0, height = v.element.height, width = v.element.width })
    |> Task.attempt
      (\result ->
        case result of
          Ok v -> ViewportUpdated v
          Err err -> DebugFail <| Debug.toString err
      )

-- CONSTANTS


lineHeightConst = 17
lineWidthConst = 200
fontSizeConst = 16
viewportCaretPadding = 20
viewportOverMoveToCaretMargin = 10
caretWidth = 2
lineNumberMarginConst = 10
lineNumberHorizontalPaddingConst = 5
caretZIndex = 100
layerSize = 1000000


-- IDS


editorId = "editor"
verticalScrollbarId = "vertical-scrollbar-outer"
horizontalScrollbarId = "horizontal-scrollbar-outer"


-- MODEL


type Msg
      = KeyboardMsgWrapper KeyboardMsg
      | TextChangedTestArea String
      | SelectionStarted Int Int
      | SelectionProgressed Int Int
      | SelectionFinished Int Int
      | None
      | DebugFail String
      | CharMeasured Float
      | ViewportUpdated ViewportInfo
      | ViewportMovedTo Float Float
      | ViewportMovedBy Float Float

type alias Model =
  { textValue : String
  , caretPosition : CaretPosition
  , highlighter : Parser (List StyledFragment)
  , selection : Maybe Selection
  , isSelectionInProgress : Bool
  , clipboard : String
  , undoStack : UndoStack
  , redoStack : RedoStack
  , charSize : Float
  , viewport : ViewportInfo
  }

type alias ViewportInfo =
  { top : Float
  , left : Float
  , height : Float
  , width : Float
  }

type alias StyledFragment =
  { from : (Int, Int)
  , class : String
  , to : (Int, Int)
  }

type alias StyledChar =
  { value : Char
  , isKeyword : Bool
  , isErrored : Bool
  }


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TextChangedTestArea text -> ({ model | textValue = text }, Cmd.none)
        KeyboardMsgWrapper keyMsg ->
          let
            newModel = updateAfterKeyboardMsg keyMsg model
          in
          scrollToCaretIfNeeded newModel
        None -> (model, Cmd.none)
        DebugFail error -> ({ model | textValue = error }, Cmd.none)
        SelectionStarted line x ->
          let
            position = convertClickedPosToCaretPos model { x = x, y = line}
            updatedModel = applyChangeWithUndo (CaretMoved { toPosition = position, withSelection = False }) model
          in
          ({ updatedModel | isSelectionInProgress = True }, Cmd.none)
        SelectionProgressed line x ->
          if model.isSelectionInProgress
            then
              let
                position = convertClickedPosToCaretPos model { x = x, y = line }
                newModel = applyChangeWithUndo (CaretMoved { toPosition = position, withSelection = True }) model
              in
              scrollToCaretIfNeeded newModel
            else
              (model, Cmd.none)
        SelectionFinished line x ->
          if model.isSelectionInProgress
            then
              let
                position = convertClickedPosToCaretPos model { x = x, y = line}
                updatedModel = applyChangeWithUndo (CaretMoved { toPosition = position, withSelection = True }) model
              in
              ({ updatedModel
              | isSelectionInProgress = False
              , selection = if Sel.isEmptySelection updatedModel.selection then Nothing else updatedModel.selection
              }, Task.attempt (always None) <| Dom.focus "editor")
            else
              (model, Cmd.none)
        CharMeasured size ->
          ({ model | charSize = size }, Cmd.none)
        ViewportUpdated viewport ->
          ({ model | viewport = viewport }, Cmd.none)
        ViewportMovedTo left top ->
          let
            viewport = model.viewport
          in
          ({ model | viewport = { viewport | left = left, top = top } }, Cmd.none)
        ViewportMovedBy left top ->
          let
            -- TODO: de-duplicate the content dimensions logic
            lines = String.lines model.textValue
            contentHeight = List.length lines * lineHeightConst
            contentWidth = List.foldl (max << ((*) model.charSize) << toFloat << String.length) 0 lines + countLineNumberFullWidth model.textValue + 20
            oldViewport = model.viewport
            newLeft =
              (oldViewport.left + left)
                |> min (contentWidth - oldViewport.width)
                |> max 0
            newTop =
              (oldViewport.top + top)
                |> min (toFloat contentHeight - oldViewport.height)
                |> max 0
            newViewport = { oldViewport | left = newLeft, top = newTop }
          in
          ({ model | viewport = newViewport }, syncScrollbar newViewport)

updateAfterKeyboardMsg : KeyboardMsg -> Model -> Model
updateAfterKeyboardMsg msg model =
  case msg of
    InsertChar c ->
      applyChangeWithUndo (TextInserted { toInsert = String.fromChar c }) model
    MoveCaretRight ->
      let
        newPos =
          case model.selection of
            Nothing -> Pos.updateCaretPosByIndexUpdate ((+) 1) model.textValue model.caretPosition
            Just sel -> Sel.secondPosition sel
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = False }) model
    MoveCaretRightWithSelection ->
      let
        newPos = Pos.updateCaretPosByIndexUpdate ((+) 1) model.textValue model.caretPosition
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    MoveCaretLeft ->
      let
        newPos =
          case model.selection of
            Nothing -> Pos.updateCaretPosByIndexUpdate (\x -> x - 1) model.textValue model.caretPosition
            Just sel -> Sel.firstPosition sel
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = False }) model
    MoveCaretLeftWithSelection ->
      let
        newPos = Pos.updateCaretPosByIndexUpdate (\x -> x - 1) model.textValue model.caretPosition
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    MoveCaretUp ->
      let
        newPos = Pos.roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line - 1)
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = False }) model
    MoveCaretUpWithSelection ->
      let
        newPos = Pos.roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line - 1)
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    MoveCaretDown ->
      let
        newPos = Pos.roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line + 1)
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = False }) model
    MoveCaretDownWithSelection ->
      let
        newPos = Pos.roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line + 1)
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    MoveCaretToLineEnd ->
      let
        newPos = Pos.roundCaretPos model.textValue <| CaretPosition maxSafeInteger model.caretPosition.line
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = False }) model
    MoveCaretToLineStart ->
      applyChangeWithUndo (CaretMoved { toPosition = CaretPosition 0 model.caretPosition.line, withSelection = False }) model
    MoveCaretToTheStart ->
      applyChangeWithUndo (CaretMoved { toPosition = CaretPosition 0 0, withSelection = False }) model
    MoveCaretToTheEnd ->
      let
        lines = String.lines model.textValue
        newPos = CaretPosition (String.length <| Maybe.withDefault "" <| last lines) (List.length lines - 1)
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = False }) model
    MoveCaretToLineEndWithSelection ->
      let
        newPos = Pos.roundCaretPos model.textValue <| CaretPosition maxSafeInteger model.caretPosition.line
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    MoveCaretToLineStartWithSelection ->
      let
        newPos = CaretPosition 0 model.caretPosition.line
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    MoveCaretToTheStartWithSelection ->
      let
        newPos = CaretPosition 0 0
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    MoveCaretToTheEndWithSelection ->
      let
       lines = String.lines model.textValue
       newPos = CaretPosition (String.length <| Maybe.withDefault "" <| last lines) (List.length lines - 1)
      in
      applyChangeWithUndo (CaretMoved { toPosition = newPos, withSelection = True }) model
    RemoveNextChar ->
      applyChangeWithUndo (CharRemoved { index = Pos.caretPosToIndex model.textValue model.caretPosition }) model
    RemovePrevChar ->
      applyChangeWithUndo (CharRemoved { index = Pos.caretPosToIndex model.textValue model.caretPosition - 1 }) model
    AddNewLine ->
      applyChangeWithUndo (TextInserted { toInsert = "\n" }) model
    Copy ->
      let
        selectedText = Maybe.map (Sel.getSelectedText model.textValue) model.selection
        line = (Maybe.withDefault "" <| EList.getAt model.caretPosition.line (String.lines model.textValue)) ++ "\n"
        toCopy =
          case selectedText of
            Nothing -> line
            Just t -> if String.length t == 0 then line else t
        isLineCopy = String.length (Maybe.withDefault "" selectedText) == 0
      in
      applyChangeWithUndo (ClipboardChanged { newClipboard = toCopy, isLineCopy = isLineCopy }) model
    Paste ->
      applyChangeWithUndo (TextInserted { toInsert = model.clipboard }) model
    Undo ->
      undoLastBatch model
    Redo ->
      redoLastBatch model

convertClickedPosToCaretPos : Model -> { x : Int, y: Int } -> CaretPosition
convertClickedPosToCaretPos model { x, y } =
  let
    lineNumberFullWidth = countLineNumberFullWidth model.textValue
    lines = String.lines model.textValue
    clickedXIndex = floor ((toFloat x - lineNumberFullWidth) / model.charSize)
    clickedLineIndex = min (List.length lines - 1) y
    clickedLine =
      lines
        |> EList.getAt y
        |> Maybe.withDefault ""
  in
  CaretPosition (min (String.length clickedLine) clickedXIndex) clickedLineIndex


countLineNumberFullWidth : String -> Float
countLineNumberFullWidth textValue =
    textValue
      |> String.lines
      |> List.length
      |> max 1
      |> countLineNumberWidth
      |> (+) lineNumberMarginConst

applyChangeWithUndo : Change -> Model -> Model
applyChangeWithUndo change model =
    model
      |> applyChangeToUndoStack change
      |> applyChange change

type alias CaretPixelPosition =
  { x : Float
  , y : Float
  }

shouldMoveLeft : ViewportInfo -> CaretPixelPosition -> Bool
shouldMoveLeft viewport caretPos =
    caretPos.x - viewport.left < viewportCaretPadding

shouldMoveRight : ViewportInfo -> CaretPixelPosition -> Bool
shouldMoveRight viewport caretPos =
    caretPos.x - viewport.left > viewport.width - viewportCaretPadding - caretWidth

shouldMoveUp : ViewportInfo -> CaretPixelPosition -> Bool
shouldMoveUp viewport caretPos =
    caretPos.y - viewport.top < viewportCaretPadding && viewport.top > 0

shouldMoveDown : ViewportInfo -> CaretPixelPosition -> Bool
shouldMoveDown viewport caretPos =
    caretPos.y - viewport.top > viewport.height - viewportCaretPadding

findNewX : ViewportInfo -> CaretPixelPosition -> Float
findNewX viewport caretPos =
  if not (shouldMoveLeft viewport caretPos || shouldMoveRight viewport caretPos)
    then
      viewport.left
    else
      if shouldMoveLeft viewport caretPos
        then caretPos.x - viewportCaretPadding - viewportOverMoveToCaretMargin + caretWidth
        else caretPos.x - viewport.width + viewportCaretPadding + viewportOverMoveToCaretMargin

findNewY : ViewportInfo -> CaretPixelPosition -> Float
findNewY viewport caretPos =
  if not (shouldMoveUp viewport caretPos || shouldMoveDown viewport caretPos)
    then
      viewport.top
    else
      if shouldMoveUp viewport caretPos
        then caretPos.y - viewportCaretPadding - viewportOverMoveToCaretMargin
        else caretPos.y - viewport.height + viewportCaretPadding + viewportOverMoveToCaretMargin

shouldMove : ViewportInfo -> CaretPixelPosition -> Bool
shouldMove viewport caretPos =
    shouldMoveLeft viewport caretPos ||
    shouldMoveRight viewport caretPos ||
    shouldMoveUp viewport caretPos ||
    shouldMoveDown viewport caretPos


moveViewportIfNecessary : ViewportInfo -> CaretPixelPosition -> ViewportInfo
moveViewportIfNecessary viewport caretPos =
    if shouldMove viewport caretPos
      then
        let
          newX = max 0 <| findNewX viewport caretPos
          newY = max 0 <| findNewY viewport caretPos
        in
        { viewport | left = newX, top = newY }
      else
        viewport

syncScrollbar : ViewportInfo -> Cmd Msg
syncScrollbar viewport =
  let
    syncHorizontalScrollbar = Dom.setViewportOf horizontalScrollbarId viewport.left 0
  in
  Dom.setViewportOf verticalScrollbarId 0 viewport.top
    |> Task.andThen (always syncHorizontalScrollbar)
    |> Task.onError (always syncHorizontalScrollbar)
    |> Task.attempt (always None)


scrollToCaretIfNeeded : Model -> (Model, Cmd Msg)
scrollToCaretIfNeeded model =
  let
    caretX = countLineNumberFullWidth model.textValue + (model.charSize * toFloat model.caretPosition.column)
    caretY = toFloat <| lineHeightConst * model.caretPosition.line
    newViewport = moveViewportIfNecessary model.viewport (CaretPixelPosition caretX caretY)
  in
  ({ model | viewport = newViewport }, if newViewport /= model.viewport then syncScrollbar newViewport else Cmd.none)

-- SUBSCRIPTIONS


subscriptions _ =
  Sub.none


-- VIEW


wrapKeyboardDecoder : Json.Decoder KeyboardMsg -> Json.Decoder (Msg, Bool)
wrapKeyboardDecoder = Json.map (\msg -> (KeyboardMsgWrapper msg, True))

countDigits : Int -> Int
countDigits number =
    if number > 0
      then 1 + countDigits (number // 10)
      else 0

countLineNumberWidth : Int -> Float
countLineNumberWidth linesNumber =
    toFloat (10 + countDigits (max 1 linesNumber) * 10)

viewPredefinedStyles : Int -> Html Msg
viewPredefinedStyles linesNumber =
    let
      lineNumberWidth = countLineNumberWidth linesNumber
    in
    global
      [ Global.class "line"
        [ minHeight (px lineHeightConst)
        , lineHeight (px lineHeightConst)
        , displayFlex
        , whiteSpace pre
        ]
      , Global.class "char-inner"
        [ transform <| translateX (pct -250)
        , backgroundColor transparent
        , display inlineBlock
        , position absolute
        , width (pct 60)
        , height (pct 100)
        , zIndex (int 5)
        ]
      , Global.class "char-outer"
        [ height (pct 100)
        , position relative
        ]
      , Global.class "line-number"
        [ height (pct 100)
        , maxWidth (px lineNumberWidth)
        , minWidth (px lineNumberWidth)
        , backgroundColor (rgb 0 0 200)
        , color (rgb 250 250 250)
        , marginRight (px lineNumberMarginConst)
        , paddingLeft (px lineNumberHorizontalPaddingConst)
        , paddingRight (px lineNumberHorizontalPaddingConst)
        , boxSizing borderBox
        ]
      , Global.class "line-selection"
        [ backgroundColor (rgb 2 190 224)
        , property "mix-blend-mode" "hue"
        , position absolute
        ]
      , Global.class "test-keyword"
        [ backgroundColor (rgb 80 80 220)
        ]
      , Global.class "test-error"
        [ backgroundColor (rgb 0 0 0)
        , color (rgb 250 80 80)
        ]
      , Global.class "highlight"
        [ pointerEvents none
        ]
      ]

clickDecoder : (Int -> Msg) -> Json.Decoder Msg
clickDecoder msgCreator =
  Json.map msgCreator
    (Json.field "offsetX" (Json.int))

normalizeLineFragmentsLoop : List StyledFragment -> List StyledFragment
normalizeLineFragmentsLoop startOrdered =
  case startOrdered of
    [] -> []
    [last] -> [last]
    first :: second :: xs -> { first | to = (endLine first, min (endCol first) (startCol second)) } :: normalizeLineFragmentsLoop (second :: xs)

startCol : StyledFragment -> Int
startCol =
  Tuple.second << .from

endCol : StyledFragment -> Int
endCol =
  Tuple.second << .to

startLine : StyledFragment -> Int
startLine =
  Tuple.first << .from

endLine : StyledFragment -> Int
endLine =
  Tuple.first << .to

view : Model -> Html Msg
view model =
    let
      lines = String.lines model.textValue
      contentHeight = List.length lines * lineHeightConst
      contentWidth = List.foldl (max << ((*) model.charSize) << toFloat << String.length) 0 lines + countLineNumberFullWidth model.textValue + 20 -- TODO remove the constant once the gutters are out
      shouldViewVerticalScrollbar = toFloat contentHeight > model.viewport.height
      shouldViewHorizontalScrollbar = contentWidth > model.viewport.width
    in
    div
      [ css
          [ whiteSpace noWrap
          , fontSize (px fontSizeConst)
          , position relative
          , focus [ backgroundColor (rgba 10 200 50 0.7) ]
          , overflowX hidden
          , overflowY hidden
          , outline none
          , fontFamily monospace
          , boxSizing borderBox
          , property "user-select" "none"
          , cursor text_
          , height (pct 100)
          , width (pct 100)
          , property "contain" "size layout paint"
          ]
      , preventDefaultOn "keydown" <| wrapKeyboardDecoder keyboardMsgDecoder
      , tabindex 0
      , id editorId
      ]
      [ viewCharSizeTest
      , viewPredefinedStyles (List.length <| String.lines model.textValue)
      , viewContent model
      , if shouldViewVerticalScrollbar
          then viewVerticalScrollbar contentHeight shouldViewHorizontalScrollbar model.viewport.left
          else div [] []
      , if shouldViewHorizontalScrollbar
          then viewHorizontalScrollbar contentWidth shouldViewVerticalScrollbar model.viewport.top
          else div [] []
      ]

scrollTopDecoder : (Float -> Msg) -> Json.Decoder Msg
scrollTopDecoder msgCreator =
  Json.map msgCreator
    (Json.at ["target", "scrollTop"] Json.float)

scrollLeftDecoder : (Float -> Msg) -> Json.Decoder Msg
scrollLeftDecoder msgCreator =
  Json.map msgCreator
    (Json.at ["target", "scrollLeft"] Json.float)

viewVerticalScrollbar : Int -> Bool -> Float -> Html Msg
viewVerticalScrollbar contentHeight areBothScrollbarsPresent scrollLeft =
  div
    [ id verticalScrollbarId
    , style "position" "absolute"
    , style "top" "0"
    , style "bottom" <| if areBothScrollbarsPresent then "11px" else "0px"
    , style "right" "0"
    , style "overflow-x" "hidden"
    , style "overflow-y" "scroll"
    , style "cursor" "text"
    , on "scroll" (scrollTopDecoder <| ViewportMovedTo scrollLeft)
    ]
    [ div
        [ id "vertical-scrollbar-inner"
        , style "min-width" "1px"
        , style "height" (String.fromInt contentHeight ++ "px")
        , on "scroll" (scrollTopDecoder <| ViewportMovedTo scrollLeft)
        ]
        [
        ]
    ]


viewHorizontalScrollbar : Float -> Bool -> Float -> Html Msg
viewHorizontalScrollbar contentWidth areBothScrollbarsPresent scrollTop =
  div
    [ id horizontalScrollbarId
    , style "position" "absolute"
    , style "left" "0"
    , style "right" <| if areBothScrollbarsPresent then "11px" else "0px"
    , style "bottom" "0"
    , style "overflow-y" "hidden"
    , style "overflow-x" "scroll"
    , style "cursor" "text"
    , on "scroll" (scrollLeftDecoder <| flip ViewportMovedTo scrollTop)
    ]
    [ div
        [ id "horizontal-scrollbar-inner"
        , style "min-height" "1px"
        , style "width" (String.fromFloat contentWidth ++ "px")
        ]
        [
        ]
    ]

wheelDecoder : (Float -> Float -> Msg) -> Json.Decoder Msg
wheelDecoder msgCreator =
  Json.map2 msgCreator
    (Json.field "deltaX" Json.float)
    (Json.field "deltaY" Json.float)

viewContent : Model -> Html Msg
viewContent model =
  div
    [ css
      [ position absolute
      , top (px 0)
      , bottom (px 0)
      , left (px 0)
      , right (px 0)
      , overflow hidden
      , property "contain" "size layout"
      ]
    , id "viewport"
    , on "wheel" (wheelDecoder ViewportMovedBy)
    ]
    [ div
        [ css
          [ position absolute
          , top (px (model.viewport.top * -1))
          , left (px (model.viewport.left * -1))
          , height (px layerSize)
          , width (px layerSize)
          , property "contain" "size layout"
          ]
        , id "content"
        ]
        [ viewTextLayer model
        , viewPositionedCaret model
        , viewSelectionOverlay model
        ]
    ]

viewPositionedCaret : Model -> Html Msg
viewPositionedCaret model =
    div
      [ id "caretPositioned"
      , css
          [ position absolute
          , top <| px (toFloat <| lineHeightConst * model.caretPosition.line)
          , left <| px (countLineNumberFullWidth model.textValue + (model.charSize * toFloat model.caretPosition.column))
          , pointerEvents none
          , zIndex (int caretZIndex)
          ]
      ]
      [ caret
      ]

viewSelectionOverlay : Model -> Html msg
viewSelectionOverlay model =
  case model.selection of
    Nothing -> div [] []
    Just selection ->
      let
        normalizedSelection = Sel.normalizeSelection selection
        lineNumWidth = countLineNumberFullWidth model.textValue
      in
      model.textValue
        |> String.lines
        |> List.indexedMap (\i line -> if i >= normalizedSelection.start.line && i <= normalizedSelection.end.line then Just line else Nothing)
        |> List.indexedMap (\i mLine -> Maybe.map (always <| viewLineSelection i lineNumWidth model.charSize normalizedSelection) mLine)
        |> List.concatMap (Maybe.withDefault [] << Maybe.map (\e -> [e]))
        |> div []

viewLineSelection : Int -> Float -> Float -> Selection -> Html msg
viewLineSelection lineNumber lineLeftOffset charWidth selection =
    let
      topPx = lineNumber * lineHeightConst
      selectionStart =
        if selection.start.line == lineNumber
          then selection.start.column
          else 0
      leftPx = lineLeftOffset + (toFloat selectionStart * charWidth)
      lineSelectionWidth =
        if selection.end.line == lineNumber
          then Px ((toFloat selection.end.column * charWidth) - leftPx + lineLeftOffset)
          else UntilLineEnd
    in
    viewSelection (toFloat topPx) leftPx lineSelectionWidth

type LineSelectionWidth
  = Px Float
  | UntilLineEnd

viewSelection : Float -> Float -> LineSelectionWidth -> Html msg
viewSelection topPx leftPx lineSelectionWidth =
    let
      widthProperty =
        case lineSelectionWidth of
          Px w -> String.fromFloat w ++ "px"
          UntilLineEnd -> "100%"
    in
    div
      [ class "line-selection"
      , style "height" (String.fromInt lineHeightConst ++ "px")
      , style "top" (String.fromFloat topPx ++ "px")
      , style "left" (String.fromFloat leftPx ++ "px")
      , style "width" widthProperty
      ]
      []

viewCharSizeTest : Html msg
viewCharSizeTest =
  span
    [ css
      [ opacity (num 0)
      , padding (px 0)
      , position absolute
      , pointerEvents none
      ]
    , id "charTester"
    ]
    [ text "mmmmmmmmmmiiiiiiiiiioooooooooowwwwwwwwwwlllllllllljjjjjjjjjj"
    ]

viewTextLayer : Model -> Html Msg
viewTextLayer model =
  let
    viewportStartLine = floor model.viewport.top // lineHeightConst
    viewportLineCount = ceiling model.viewport.height // lineHeightConst + 5
  in
  div
    [ css
      [ position absolute
      , top (px 0)
      , left (px 0)
      , height (px layerSize)
      , width (px layerSize)
      , property "contain" "size layout"
      ]
    , id "textLayer"
    ]
    <| ( model.textValue
          |> String.lines
          |> (List.take viewportLineCount << List.drop viewportStartLine)
          |> List.indexedMap (\i l -> lazy3 viewEditorLineWithCaret model.highlighter (i + viewportStartLine) l)
          |> (\lst ->
              if List.isEmpty lst
                then [viewEditorLineWithCaret model.highlighter 0 ""]
                else lst))

type alias ViewLineParams =
  { isSelectionInProgress : Bool
  , selection : Maybe Selection
  , caretPositionOnLine : Maybe Int
  , lineNumber : Int
  , chars : List StyledChar
  , isLastLine : Bool
  }

viewEditorLineWithCaret : Parser (List StyledFragment) -> Int -> String -> Html Msg
viewEditorLineWithCaret parser lineNumber lineContent =
    let
      fragments =
        Parser.run parser lineContent
          |> Result.withDefault []
          |> List.sortBy startCol
    in
    div
      [ id <| "line " ++ String.fromInt lineNumber
      , class "line"
      , on "mousedown" <| clickDecoder <| SelectionStarted lineNumber
      , on "mousemove" <| clickDecoder <| SelectionProgressed lineNumber
      , on "mouseup" <| clickDecoder <| SelectionFinished lineNumber
      , style "position" "absolute"
      , style "top" (String.fromInt (lineNumber * lineHeightConst) ++ "px")
      , style "left" "0"
      , style "right" "0"
      , style "contain" "size layout"
      ]
      (viewLineNumber lineNumber
      :: viewLineContent 0 lineContent fragments
      )

viewLineContent : Int -> String -> List StyledFragment -> List (Html Msg)
viewLineContent index str normalizedFragments =
    case (normalizedFragments, index >= String.length str) of
      (_, True) ->
        []
      ([], _) ->
        [text <| String.slice index (String.length str) str]
      (fr :: restFrs, _) ->
        if startCol fr - 1 > index
          then
            text (String.slice index (startCol fr - 1) str) :: viewLineContent (startCol fr - 1) str (fr :: restFrs)
          else
            span
              [ class ("highlight " ++ fr.class)
              ]
              [ text <| String.slice index (endCol fr - 1) str
              ]
              :: viewLineContent (endCol fr - 1) str restFrs



--alwaysStopPropagation : String -> Msg -> Html.Styled.Attribute Msg
--alwaysStopPropagation event msg =
--  stopPropagationOn event (Json.succeed (msg, True))

viewLineNumber : Int -> Html Msg
viewLineNumber num =
    span
      [ class "line-number"
      ]
      [ text <| String.fromInt num
      ]

splitBy : (a -> Bool) -> List a -> List (List a)
splitBy isDivider list =
    let
      continue lst = (takeWhile (not << isDivider) lst) :: splitBy isDivider (dropWhile (not << isDivider) lst)
    in
    case EList.uncons list of
      Nothing -> []
      Just (first, []) -> if isDivider first then [[]] else [[first]]
      Just (first, rest) -> if isDivider first then continue rest else continue list

--insertOnIndex : Int -> a -> List a -> List a
--insertOnIndex index element lst =
--  List.take index lst ++ element :: List.drop index lst
--
--insertOnMaybeIndex : Maybe Int -> a -> List a -> List a
--insertOnMaybeIndex maybeIndex elem list =
--    case maybeIndex of
--      Nothing -> list
--      Just index -> insertOnIndex index elem list

caret =
  span
    [ css
        [ display block
        , height (px 15)
        , width (px caretWidth)
        , backgroundColor (rgb 200 20 0)
        , animationName <| keyframes [ (0, [ Animation.opacity (int 100) ]), (100, [ Animation.opacity (int 0) ]) ]
        , property "animation-iteration-count" "infinite"
        , animationDuration (sec 1)
        , focus [ backgroundColor (rgb 50 100 220), width (px 3) ]
        ]
    , id "caret-inner"
    , tabindex 0
    ] []
