module Main exposing (..)
import Basics.Extra exposing (maxSafeInteger)
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

main : Program () Model Msg
main =
    Browser.element
      { init = always (
                { textValue = "empty"
                , isSelectionInProgress = False
                , caretPosition = CaretPosition 0 0
                , selection = Nothing
                , highlighter = testParser
                , clipboard = ""
                , undoStack = []
                , redoStack = []
                , charSize = 0
                }, measureCharSize)
      , update = update
      , view = view >> toUnstyled
      , subscriptions = subscriptions
      }

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

testParser =
  Parser.loop [] testParserStep

type alias Located a =
  { from : (Int, Int)
  , value : a
  , to : (Int, Int)
  }

located : Parser a -> Parser { from : (Int, Int), to : (Int, Int), value: a }
located parser =
  Parser.succeed Located
    |= Parser.getPosition
    |= parser
    |= Parser.getPosition

parseToken : String -> String -> List StyledFragment -> Parser (Parser.Step (List StyledFragment) (List StyledFragment))
parseToken token class tmpResults =
  located (Parser.succeed class |. Parser.token token)
    |> Parser.map (\found -> Parser.Loop <| StyledFragment found.from class found.to :: tmpResults)

testParserStep : List StyledFragment -> Parser (Parser.Step (List StyledFragment) (List StyledFragment))
testParserStep results =
  Parser.oneOf
    [ parseToken "lol" "test-keyword" results
    , parseToken "lolz" "test-error" results
    , Parser.end |> Parser.map (always <| Parser.Done results)
    , Parser.chompIf (always True)
      |> Parser.map (always <| Parser.Loop results)
    ]


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
          (updateAfterKeyboardMsg keyMsg model, scrollToCaretIfNeeded)
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
                position = convertClickedPosToCaretPos model { x = x, y = line}
              in
              (applyChangeWithUndo (CaretMoved { toPosition = position, withSelection = True }) model, scrollToCaretIfNeeded)
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

shouldMoveLeft : Element -> Bool
shouldMoveLeft el =
    el.element.x - el.viewport.x < viewportCaretPadding

shouldMoveRight : Element -> Bool
shouldMoveRight el =
    el.element.x - el.viewport.x > el.viewport.width - viewportCaretPadding - caretWidth

shouldMoveUp : Element -> Bool
shouldMoveUp el =
    el.element.y - el.viewport.y < viewportCaretPadding

shouldMoveDown : Element -> Bool
shouldMoveDown el =
    el.element.y - el.viewport.y > el.viewport.height - viewportCaretPadding

findNewX : Element -> Float
findNewX el =
  if not (shouldMoveLeft el || shouldMoveRight el)
    then
      el.viewport.x
    else
      if shouldMoveLeft el
        then el.element.x - viewportCaretPadding - viewportOverMoveToCaretMargin + caretWidth
        else el.element.x - el.viewport.width + viewportCaretPadding + viewportOverMoveToCaretMargin

findNewY : Element -> Float
findNewY el =
  if not (shouldMoveUp el || shouldMoveDown el)
    then
      el.viewport.y
    else
      if shouldMoveUp el
        then el.element.y - viewportCaretPadding - viewportOverMoveToCaretMargin
        else el.element.y - el.viewport.height + viewportCaretPadding + viewportOverMoveToCaretMargin

shouldMove : Element -> Bool
shouldMove el =
    shouldMoveLeft el || shouldMoveRight el || shouldMoveUp el || shouldMoveDown el

moveViewportIfNecessary : Element -> Task.Task Dom.Error ()
moveViewportIfNecessary el =
    if shouldMove el
      then Dom.setViewportOf "editor" (findNewX el) (findNewY el)
      else Task.succeed ()

createRelativeElement : { element: Element, viewport: Dom.Viewport, viewportElement: Element } -> Element
createRelativeElement { element, viewport, viewportElement } =
    let
      elem = element.element
    in
    { scene = viewport.scene
    , viewport = viewport.viewport
    , element =
        { elem
        | x = elem.x - viewportElement.element.x + viewport.viewport.x
        , y = elem.y - viewportElement.element.y + viewport.viewport.y
        }
    }

scrollToCaretIfNeeded : Cmd Msg
scrollToCaretIfNeeded =
    Task.map3
      (\element viewport editor -> createRelativeElement { element = element, viewport = viewport, viewportElement = editor })
      (Dom.getElement "caret-inner")
      (Dom.getViewportOf "editor")
      (Dom.getElement "editor")
      |> Task.andThen moveViewportIfNecessary
      |> Task.attempt (always None)
      --|> Task.attempt (\result ->
      --                    case result of
      --                      Ok _ -> Debug.log "successful task" None
      --                      Err err -> DebugFail <| Debug.toString err
      --                )


-- SUBSCRIPTIONS


subscriptions _ =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    div
      []
      [ br [] []
      , viewTestArea model
      , div
        [ css
          [ height (px 300)
          , width (px 400)
          , marginLeft (px 50)
          ]
        ]
        [ viewEditor model
        ]
      ]

viewTestArea model =
  textarea
    [ onInput TextChangedTestArea
    , css [ width <| px 100, height <| px 100, backgroundColor <| rgb 100 200 0 ]
    ]
    [ text model.textValue
    ]

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


--createPerLineDict : List StyledFragment -> Dict Int (List StyledFragment)
--createPerLineDict fragments =
--    fragments
--      |> List.foldl addFragmentToDict Dict.empty

--addFragmentToDict : StyledFragment -> Dict Int (List StyledFragment) -> Dict Int (List StyledFragment)
--addFragmentToDict fragment dict =
--    dict
--      |> Dict.update (startLine fragment) (addToDictList fragment)
--      |> (\x -> if startLine fragment == endLine fragment then x else Dict.update (endLine fragment) (addToDictList fragment) x)

--addToDictList : StyledFragment -> Maybe (List StyledFragment) -> Maybe (List StyledFragment)
--addToDictList fragment mList =
--    case mList of
--      Nothing -> Just [fragment]
--      Just list -> Just (fragment :: list)

--normalizeLineFragments : Int -> String -> List StyledFragment -> List StyledFragment
--normalizeLineFragments lineNum line fragments =
--    fragments
--      |> List.map (\f -> { f | from = normalizeFromPos lineNum f.from, to = normalizeToPos lineNum line f.to })
--      |> List.sortBy startCol
--      |> normalizeLineFragmentsLoop

--normalizeFromPos : Int -> (Int, Int) -> (Int, Int)
--normalizeFromPos lineNum (line, col) =
--  (lineNum, if line < lineNum then 0 else max 0 col)
--
--normalizeToPos : Int -> String -> (Int, Int) -> (Int, Int)
--normalizeToPos lineNum lineStr (line, col) =
--  let
--    lineLen = String.length lineStr
--  in
--  (lineNum, if line > lineNum then lineLen else min lineLen col)

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

viewEditor : Model -> Html Msg
viewEditor model =
    div
        [ css
            [ whiteSpace noWrap
            , fontSize (px fontSizeConst)
            , position relative
            , focus [ backgroundColor (rgba 10 200 50 0.7) ]
            , overflowX auto
            , overflowY auto
            , outline none
            , fontFamily monospace
            , boxSizing borderBox
            , property "user-select" "none"
            , cursor text_
            , height (pct 100)
            , width (pct 100)
            ]
          , preventDefaultOn "keydown" <| wrapKeyboardDecoder keyboardMsgDecoder
          , tabindex 0
        , id "editor"
        ]
        <| viewCharSizeTest
        :: viewPredefinedStyles (List.length <| String.lines model.textValue)
        :: viewPositionedCaret model
        :: viewSelectionOverlay model
        :: [div [] ( model.textValue
            |> String.lines
            |> List.indexedMap (lazy3 viewEditorLineWithCaret model.highlighter)
            |> (\lst ->
                if List.isEmpty lst
                  then [viewEditorLineWithCaret model.highlighter 0 ""]
                  else lst))]

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
      , style "contain" "style size layout"
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