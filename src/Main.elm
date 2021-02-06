module Main exposing (..)
import Array exposing (Array)
import Basics.Extra exposing (flip, maxSafeInteger)
import Browser
import Browser.Dom as Dom exposing (Element)
import Change exposing (Change(..), applyChange)
import Css exposing (..)
import Css.Animations as Animation exposing (keyframes)
import Html.Styled.Events exposing (onClick, onInput, onMouseDown, onMouseOver, onMouseUp, preventDefaultOn, stopPropagationOn)
import Html.Styled exposing (Html, br, div, span, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Json.Decode as Json
import KeyboardMsg exposing (KeyboardMsg(..), keyboardMsgDecoder)
import List.Extra as EList exposing (dropWhile, last, takeWhile)
import Parser exposing ((|=), Parser)
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
                }, Cmd.none)
      , update = update
      , view = view >> toUnstyled
      , subscriptions = subscriptions
      }

testKeyword str =
  Parser.keyword str
  |> Parser.andThen (always <| Parser.succeed <| List.map keywordedChar <| String.toList str)
  |> Parser.map Array.fromList

testError str =
  Parser.keyword str
  |> Parser.andThen (always <| Parser.succeed <| List.map erroredChar <| String.toList str)
  |> Parser.map Array.fromList

testParser =
  Parser.loop Array.empty testParserStep
    |> Parser.map Array.toList

appendingParserLoop : Array a -> Parser (Array a -> Parser.Step (Array a) (Array a))
appendingParserLoop arrResults = Parser.succeed (Parser.Loop << Array.append arrResults)
anyCharTaker = Parser.chompIf (always True)
                |> Parser.getChompedString
                |> Parser.map (String.toList >> List.map plainChar >> Array.fromList)

testParserStep : Array StyledChar -> Parser (Parser.Step (Array StyledChar) (Array StyledChar))
testParserStep arrResults =
  Parser.oneOf
    [ appendingParserLoop arrResults
        |= testKeyword "lol"
    , appendingParserLoop arrResults
        |= testError "lolz"
    , appendingParserLoop arrResults
        |= anyCharTaker
    , Parser.end |> Parser.map (always <| Parser.Done arrResults)
    ]

keywordedChar c = StyledChar c True False
erroredChar c = StyledChar c False True
plainChar c = StyledChar c False False


-- CONSTANTS


lineHeightConst = 17
lineWidthConst = 200
fontSizeConst = 16
paddingSize = 10
viewportCaretPadding = 20
viewportOverMoveToCaretMargin = 10
caretWidth = 2


-- MODEL


type Msg
      = KeyboardMsgWrapper KeyboardMsg
      | TextChangedTestArea String
      | ClickChar Int Int
      | SelectionStarted Int Int
      | SelectionProgressed Int Int
      | SelectionFinished Int Int
      | None
      | DebugFail String

type alias Model =
  { textValue : String
  , caretPosition : CaretPosition
  , highlighter : Parser (List StyledChar)
  , selection : Maybe Selection
  , isSelectionInProgress : Bool
  , clipboard : String
  , undoStack : UndoStack
  , redoStack : RedoStack
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
        ClickChar column line ->
          ( applyChangeWithUndo (CaretMoved { toPosition = CaretPosition column line, withSelection = False }) model
          , scrollToCaretIfNeeded
          )
        KeyboardMsgWrapper keyMsg -> (updateAfterKeyboardMsg keyMsg model, scrollToCaretIfNeeded)
        None -> (model, Cmd.none)
        DebugFail error -> ({ model | textValue = error }, Cmd.none)
        SelectionStarted column line ->
          let
            position = CaretPosition column line
            updatedModel = applyChangeWithUndo (CaretMoved { toPosition = position, withSelection = False }) model
          in
          ({ updatedModel | isSelectionInProgress = True }, Cmd.none)
        SelectionProgressed column line ->
          let
            position = CaretPosition column line
          in
          (applyChangeWithUndo (CaretMoved { toPosition = position, withSelection = True }) model, scrollToCaretIfNeeded)
        SelectionFinished column line ->
          let
            position = CaretPosition column line
            updatedModel = applyChangeWithUndo (CaretMoved { toPosition = position, withSelection = True }) model
          in
          ({ updatedModel | isSelectionInProgress = False }, Task.attempt (always None) <| Dom.focus "editor")

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

findNewX : Element -> Float
findNewX el =
    if shouldMoveLeft el
      then el.element.x - viewportCaretPadding - viewportOverMoveToCaretMargin + caretWidth
      else el.element.x - el.viewport.width + viewportCaretPadding + viewportOverMoveToCaretMargin

shouldMove : Element -> Bool
shouldMove el =
    shouldMoveLeft el || shouldMoveRight el

moveViewportIfNecessary : Element -> Task.Task Dom.Error ()
moveViewportIfNecessary el =
    if shouldMove el
      then Dom.setViewportOf "editor" (findNewX el) el.viewport.y
      else Task.succeed ()

createRelativeElement : { element: Element, viewport: Dom.Viewport, viewportElement: Element } -> Element
createRelativeElement { element, viewport, viewportElement } =
    let
      elem = element.element
    in
    { scene = viewport.scene
    , viewport = viewport.viewport
    , element =
        { elem | x = elem.x - viewportElement.element.x + viewport.viewport.x
        , y = elem.y - viewportElement.element.y + viewport.viewport.y
        }
    }

scrollToCaretIfNeeded : Cmd Msg
scrollToCaretIfNeeded =
    Task.map3
      (\element viewport editor -> createRelativeElement { element = element, viewport = viewport, viewportElement = editor })
      (Dom.getElement "caretChar")
      (Dom.getViewportOf "editor")
      (Dom.getElement "editor")
      |> Task.andThen moveViewportIfNecessary
      |> Task.attempt (always None)


-- SUBSCRIPTIONS


subscriptions _ =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ br [] []
        , viewTestArea model
        , viewEditor model
        ]

viewTestArea model =
  textarea
    [ onInput TextChangedTestArea
    , css [ width <| px 300, height <| px 300, backgroundColor <| rgb 100 200 0 ]
    ]
    [ text model.textValue
    ]

wrapKeyboardDecoder : Json.Decoder KeyboardMsg -> Json.Decoder (Msg, Bool)
wrapKeyboardDecoder = Json.map (\msg -> (KeyboardMsgWrapper msg, True))

viewEditor : Model -> Html Msg
viewEditor model =
  let
    parsedLines =
      Parser.run model.highlighter model.textValue
        |> Result.withDefault []
        |> splitBy isNewLine
  in
  div
    [ css
      [ padding (px paddingSize)
      , fontFamily monospace
      , overflow hidden
      , boxSizing borderBox
      , property "user-select" "none"
      , cursor text_
      ]
    ]
    [ div
        [ css
            [ whiteSpace noWrap
            , fontSize (px fontSizeConst)
            , maxWidth (px lineWidthConst)
            , position relative
            , focus [ backgroundColor (rgba 10 200 50 0.7) ]
            , overflowX auto
            , outline none
            ]
          , preventDefaultOn "keydown" <| wrapKeyboardDecoder keyboardMsgDecoder
          , tabindex 0
        , id "editor"
        ]
        <| List.indexedMap
            (\i v ->
              viewEditorLineWithCaret
                { isSelectionInProgress = model.isSelectionInProgress
                , selection = model.selection
                , caretPositionOnLine = if i == model.caretPosition.line then Just model.caretPosition.column else Nothing
                , lineNumber = i
                , isLastLine = i == List.length parsedLines - 1
                , chars = v
                }
            )
            parsedLines
    ]

isNewLine : StyledChar -> Bool
isNewLine char =
    char.value == '\n'

type alias ViewLineParams =
  { isSelectionInProgress : Bool
  , selection : Maybe Selection
  , caretPositionOnLine : Maybe Int
  , lineNumber : Int
  , chars : List StyledChar
  , isLastLine : Bool
  }

viewEditorLineWithCaret : ViewLineParams -> Html Msg
viewEditorLineWithCaret { isSelectionInProgress, selection, caretPositionOnLine, chars, isLastLine, lineNumber } =
    let
      hasCaret =
        caretPositionOnLine /= Nothing
      caretPos =
        Maybe.withDefault 0 caretPositionOnLine
      lineEvents =
        if isSelectionInProgress
          then
            [ onMouseOver <| SelectionProgressed (List.length chars) lineNumber
            , onMouseUp <| SelectionFinished (List.length chars) lineNumber
            ]
          else
            [ onMouseDown <| SelectionStarted (List.length chars) lineNumber
            ]
      isNewLineSelected = not isLastLine && Sel.isPositionSelected selection (CaretPosition (List.length chars) lineNumber)
    in
    div
      (
      [ id <| "line " ++ String.fromInt lineNumber
      , css
        [ minHeight (px lineHeightConst)
        , lineHeight (px <| lineHeightConst + 3)
        , displayFlex
        , whiteSpace pre
        ]
      ] ++ lineEvents
      )
      <| viewLineNumber lineNumber
      ::  ( chars
            |> List.indexedMap (\i v -> (v, Sel.isPositionSelected selection { column = i, line = lineNumber }))
            |> List.map viewChar
            |> List.indexedMap (\i v -> (createCharAttributes (hasCaret && (i == caretPos - 1 || (i == 0 && caretPos == 0))) isSelectionInProgress <| CaretPosition i lineNumber) <| v )
            |> insertOnMaybeIndex caretPositionOnLine caretWrapper
            |> if isNewLineSelected
                then flip List.append [selectedNewLine]
                else identity
          )

alwaysStopPropagation : String -> Msg -> Html.Styled.Attribute Msg
alwaysStopPropagation event msg =
  stopPropagationOn event (Json.succeed (msg, True))

createCharAttributes : Bool -> Bool -> CaretPosition -> Html Msg -> Html Msg
createCharAttributes hasCaret isSelectionInProgress charPos contents =
    let
      clickSelectionAttrs =
        if isSelectionInProgress
          then
            [ alwaysStopPropagation "mouseup" (SelectionFinished (charPos.column) charPos.line)
            , alwaysStopPropagation "mouseover" (SelectionProgressed (charPos.column) charPos.line)
            ]
          else [ alwaysStopPropagation "mousedown" (SelectionStarted charPos.column charPos.line) ]
      separatorAttrs =
        if not isSelectionInProgress
          then []
          else
            [ alwaysStopPropagation "mouseenter" (SelectionProgressed (charPos.column) charPos.line)
            , alwaysStopPropagation "mouseup" (SelectionFinished (charPos.column) charPos.line)
            , alwaysStopPropagation "mousedown" (SelectionStarted charPos.column charPos.line)
            ]
    in
    span
      ([ onClick (ClickChar charPos.column charPos.line)
      , id (if hasCaret then "caretChar" else "")
      , css
          [ height (pct 100)
          , position relative
          ]
      ] ++ clickSelectionAttrs)
      [ contents
      , span
          ([ id "pseudo-clicker"
          , css
              [ transform <| translateX (pct -250)
              , backgroundColor transparent
              , display inlineBlock
              , position absolute
              , width (pct 60)
              , height (pct 100)
              , zIndex (int 5)
              ]
          ] ++ separatorAttrs)
          []
      ]

viewLineNumber : Int -> Html Msg
viewLineNumber num =
    span
      [ css
        [ height (pct 100)
        , width (px 50)
        , backgroundColor (rgb 0 0 200)
        , color (rgb 250 250 250)
        , marginRight (px 10)
        , flexBasis (px 0)
        ]
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

viewChar : (StyledChar, Bool) -> Html Msg
viewChar (char, isSelected) =
    let
      renderedChar = text <| String.fromChar char.value
      charWithSelection = if isSelected then selected renderedChar else renderedChar
    in
    case (char.isKeyword, char.isErrored) of
      (True, True) -> keyword <| errored charWithSelection
      (True, False) -> keyword charWithSelection
      (False, True) -> errored charWithSelection
      (False, False) -> charWithSelection

insertOnIndex : Int -> a -> List a -> List a
insertOnIndex index element lst =
  List.take index lst ++ element :: List.drop index lst

insertOnMaybeIndex : Maybe Int -> a -> List a -> List a
insertOnMaybeIndex maybeIndex elem list =
    case maybeIndex of
      Nothing -> list
      Just index -> insertOnIndex index elem list

caretWrapper : Html Msg
caretWrapper =
    div
      [ id "caret"
      , css
          [ width (px 0)
          , display inlineBlock
          , position relative
          ]
      ]
      [ caret
      ]

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

keyword word =
  span
    [ css [ color <| rgb 242 151 5, whiteSpace preLine ] ]
    [ word ]

selected word =
  span
    [ css [ backgroundColor <| rgb 2 190 224 ] ]
    [ span [ css [property "mix-blend-mode" "hue"] ][ word ] ]

selectedNewLine =
  div
    [ css
        [ backgroundColor <| rgb 2 190 224
        , flexGrow <| int 1
        ]
    ]
    []

errored word =
  span
    [ css [ textDecoration3 underline wavy <| rgb 250 0 0 ] ]
    [ word ]