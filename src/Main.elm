module Main exposing (..)
import Array exposing (Array)
import Basics.Extra exposing (maxSafeInteger)
import Browser
import Browser.Dom as Dom exposing (Element)
import Css exposing (..)
import Css.Animations as Animation exposing (keyframes)
import Html.Styled.Events exposing (on, onClick, onInput, onMouseDown, onMouseEnter, onMouseOver, onMouseUp, preventDefaultOn, stopPropagationOn)
import Html.Styled exposing (Html, br, div, span, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Json.Decode as Json
import KeyboardMsg exposing (KeyboardMsg(..), keyboardMsgDecoder)
import List.Extra as EList exposing (dropWhile, last, takeWhile)
import Parser exposing ((|=), Parser)
import String
import Task as Task

main : Program () Model Msg
main =
    Browser.element
      { init = always (
                { textValue = "empty"
                , isSelectionInProgress = False
                , caretPosition = CaretPosition 0 0
                , selection = Nothing
                , highlighter = testParser
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
      | TextChanged String
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
  }

type alias Selection =
  { start : CaretPosition
  , end : CaretPosition
  }

type alias CaretPosition =
  { column : Int
  , line : Int
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
        TextChanged text -> ({ model | textValue = text }, Cmd.none)
        ClickChar column line -> ({ model | caretPosition = CaretPosition column line, selection = Nothing }, Cmd.none)
        KeyboardMsgWrapper keyMsg -> (updateAfterKeyboardMsg keyMsg model, scrollToCaretIfNeeded)
        None -> (model, Cmd.none)
        DebugFail error -> ({ model | textValue = error }, Cmd.none)
        SelectionStarted column line ->
          let
            position = CaretPosition column line
          in
          ({ model
          | selection = Just { start = position, end = position }
          , isSelectionInProgress = True
          , caretPosition = position
          }, Cmd.none
          )
        SelectionProgressed column line ->
          let
            position = CaretPosition column line
          in
          ({ model
          | selection =
              case model.selection of
                Nothing -> Nothing
                Just sel -> Just { sel | end = position }
          , caretPosition = position
          }, Cmd.none
          )
        SelectionFinished column line ->
          let
            position = CaretPosition column line
          in
          ({ model
          | selection =
              case model.selection of
                Nothing -> Nothing
                Just sel -> if sel.start == position then Nothing else Just { sel | end = position }
          , isSelectionInProgress = False
          , caretPosition = position
          }, Cmd.none
          )

updateAfterKeyboardMsg : KeyboardMsg -> Model -> Model
updateAfterKeyboardMsg msg model =
  case msg of
    InsertChar c ->
      let
        charPos =
          case model.selection of
            Nothing -> model.caretPosition
            Just sel -> firstPosition sel
      in
      { model
      | textValue = insertCharAt (caretPosToIndex model.textValue charPos) c <| removeSelectedText model.selection model.textValue
      , caretPosition = CaretPosition (charPos.column + 1) charPos.line
      , selection = Nothing
      }
    MoveCaretRight ->
      { model
      | caretPosition =
          case model.selection of
            Nothing -> updateCaretPosByIndexUpdate ((+) 1) model.textValue model.caretPosition
            Just sel -> secondPosition sel
      , selection = Nothing
      }
    MoveCaretRightWithSelection ->
      let
        newCaretPos = updateCaretPosByIndexUpdate ((+) 1) model.textValue model.caretPosition
      in
      { model
      | caretPosition = newCaretPos
      , selection = updateSelectionAfterCaretMoveWithSelection model.caretPosition model.selection newCaretPos
      }
    MoveCaretLeft ->
      { model
      | caretPosition =
          case model.selection of
            Nothing -> updateCaretPosByIndexUpdate (\x -> x - 1) model.textValue model.caretPosition
            Just sel -> firstPosition sel
      , selection = Nothing
      }
    MoveCaretLeftWithSelection ->
      let
        newCaretPos = updateCaretPosByIndexUpdate (\x -> x - 1) model.textValue model.caretPosition
      in
      { model
      | caretPosition = newCaretPos
      , selection = updateSelectionAfterCaretMoveWithSelection model.caretPosition model.selection newCaretPos
      }
    MoveCaretUp ->
      { model
      | caretPosition = roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line - 1)
      , selection = Nothing
      }
    MoveCaretUpWithSelection ->
      let
        newCaretPos = roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line - 1)
      in
      { model
      | caretPosition = newCaretPos
      , selection = updateSelectionAfterCaretMoveWithSelection model.caretPosition model.selection newCaretPos
      }
    MoveCaretDown ->
      { model
      | caretPosition = roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line + 1)
      , selection = Nothing
      }
    MoveCaretDownWithSelection ->
      let
        newCaretPos = roundCaretPos model.textValue <| CaretPosition model.caretPosition.column (model.caretPosition.line + 1)
      in
      { model
      | caretPosition = newCaretPos
      , selection = updateSelectionAfterCaretMoveWithSelection model.caretPosition model.selection newCaretPos
      }
    MoveCaretToLineEnd ->
      { model
      | caretPosition = roundCaretPos model.textValue <| CaretPosition maxSafeInteger model.caretPosition.line
      , selection = Nothing
      }
    MoveCaretToLineStart ->
      { model
      | caretPosition = CaretPosition 0 model.caretPosition.line
      , selection = Nothing
      }
    MoveCaretToTheStart ->
      { model
      | caretPosition = CaretPosition 0 0
      , selection = Nothing
      }
    MoveCaretToTheEnd ->
      { model
      | caretPosition =
          let
            lines = String.lines model.textValue
          in
          CaretPosition (String.length <| Maybe.withDefault "" <| last lines) (List.length lines - 1)
      , selection = Nothing
      }
    MoveCaretToLineEndWithSelection ->
      let
        newPos = roundCaretPos model.textValue <| CaretPosition maxSafeInteger model.caretPosition.line
      in
      { model
      | caretPosition = newPos
      , selection = Just <| extendSelectionTo newPos <| Maybe.withDefault (Selection model.caretPosition model.caretPosition) model.selection
      }
    MoveCaretToLineStartWithSelection ->
      let
        newPos = CaretPosition 0 model.caretPosition.line
      in
      { model
      | caretPosition = newPos
      , selection = Just <| extendSelectionTo newPos <| Maybe.withDefault (Selection model.caretPosition model.caretPosition) model.selection
      }
    MoveCaretToTheStartWithSelection ->
      let
        newPos = CaretPosition 0 0
      in
      { model
      | caretPosition = newPos
      , selection = Just <| extendSelectionTo newPos <| Maybe.withDefault (Selection model.caretPosition model.caretPosition) model.selection
      }
    MoveCaretToTheEndWithSelection ->
      let
       lines = String.lines model.textValue
       newPos = CaretPosition (String.length <| Maybe.withDefault "" <| last lines) (List.length lines - 1)
      in
      { model
      | caretPosition = newPos
      , selection = Just <| extendSelectionTo newPos <| Maybe.withDefault (Selection model.caretPosition model.caretPosition) model.selection
      }
    RemoveNextChar ->
      { model
      | textValue =
          case model.selection of
            Nothing -> removeCharAt (caretPosToIndex model.textValue model.caretPosition) model.textValue
            Just _ -> removeSelectedText model.selection model.textValue
      , caretPosition =
          case model.selection of
            Nothing -> model.caretPosition
            Just sel -> firstPosition sel
      , selection = Nothing
      }
    RemovePrevChar ->
      { model
      | textValue =
          case model.selection of
            Nothing -> removeCharAt (caretPosToIndex model.textValue model.caretPosition - 1) model.textValue
            Just _ -> removeSelectedText model.selection model.textValue
      , caretPosition =
          case model.selection of
            Nothing -> updateCaretPosByIndexUpdate (\x -> x - 1) model.textValue model.caretPosition
            Just sel -> firstPosition sel
      , selection = Nothing
      }
    AddNewLine ->
      let
        charPos =
          case model.selection of
            Nothing -> model.caretPosition
            Just sel -> firstPosition sel
      in
      { model
      | textValue = insertCharAt (caretPosToIndex model.textValue charPos) '\n' <| removeSelectedText model.selection model.textValue
      , caretPosition = CaretPosition 0 (charPos.line + 1)
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
    lineNum = max 0 <| min (List.length lines) caretPos.line
  in
  case EList.getAt lineNum lines of
    Nothing -> Debug.log "Failed to find selected line. :O " <| CaretPosition 0 0
    Just line -> CaretPosition (max 0 <| min (String.length line) caretPos.column) lineNum


removeCharAt : Int -> String -> String
removeCharAt pos str =
    String.left pos str ++ String.dropLeft (pos + 1) str

insertCharAt : Int -> Char -> String -> String
insertCharAt pos char str =
    String.left pos str ++ String.fromChar char ++ String.dropLeft pos str

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

isReversed : Selection -> Bool
isReversed selection =
    selection.end.line < selection.start.line ||
    (selection.end.line == selection.start.line && selection.end.column < selection.start.column)

isPositionSelected : Maybe Selection -> CaretPosition -> Bool
isPositionSelected maybeSelection { line, column } =
    let
      selection = Maybe.withDefault { start = CaretPosition 0 0, end = CaretPosition 0 0 } maybeSelection
      { end, start } = if isReversed selection then { start = selection.end, end = selection.start } else selection
      isOnelineSelection = end.line == start.line
    in
    (isOnelineSelection && line == end.line && end.column > column && start.column <= column) ||
    (end.line > line && start.line < line) ||
    (not isOnelineSelection && end.line == line && end.column > column) ||
    (not isOnelineSelection && start.line == line && start.column <= column)

updateSelectionAfterCaretMoveWithSelection : CaretPosition -> Maybe Selection -> CaretPosition -> Maybe Selection
updateSelectionAfterCaretMoveWithSelection oldCaretPos oldSelection newCaretPos =
    case oldSelection of
      Nothing -> if oldCaretPos == newCaretPos then Nothing else Just { start = oldCaretPos, end = newCaretPos}
      Just prev -> if prev.start == newCaretPos then Nothing else Just { start = prev.start, end = newCaretPos }

removeSelectedText : Maybe Selection -> String -> String
removeSelectedText selection text =
    case selection of
      Nothing -> text
      Just sel ->
        let
          correctedSel = if isReversed sel then { start = sel.end, end = sel.start } else sel
          startIndex = caretPosToIndex text correctedSel.start
          endIndex = caretPosToIndex text correctedSel.end
        in
        String.left startIndex text ++ String.dropLeft endIndex text

firstPosition : Selection -> CaretPosition
firstPosition selection =
    if isReversed selection then selection.end else selection.start

secondPosition : Selection -> CaretPosition
secondPosition selection =
    if isReversed selection then selection.start else selection.end

extendSelectionTo : CaretPosition -> Selection -> Selection
extendSelectionTo pos selection =
    { selection | end = pos }


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
    [ onInput TextChanged
    , css [ width <| px 300, height <| px 300, backgroundColor <| rgb 100 200 0 ]
    ]
    [ text model.textValue
    ]

wrapKeyboardDecoder : Json.Decoder KeyboardMsg -> Json.Decoder (Msg, Bool)
wrapKeyboardDecoder = Json.map (\msg -> (KeyboardMsgWrapper msg, True))

viewEditor : Model -> Html Msg
viewEditor model =
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
        ( Parser.run model.highlighter model.textValue
            |> Result.withDefault []
            |> splitBy isNewLine
            |> List.indexedMap (\i v -> viewEditorLineWithCaret model.isSelectionInProgress model.selection (if i == model.caretPosition.line then Just model.caretPosition.column else Nothing) i v)
        )
    ]

isNewLine : StyledChar -> Bool
isNewLine char =
    char.value == '\n'

viewEditorLineWithCaret : Bool -> Maybe Selection -> Maybe Int -> Int -> List StyledChar -> Html Msg
viewEditorLineWithCaret isSelectionInProgress selection maybeCaretPos num chars =
    let
      hasCaret = maybeCaretPos /= Nothing
      caretPos = Maybe.withDefault 0 maybeCaretPos
      lineEvents = if isSelectionInProgress
        then
          [ onMouseOver <| SelectionProgressed (List.length chars) num
          , onMouseUp <| SelectionFinished (List.length chars) num
          ]
        else [ onMouseDown <| SelectionStarted (List.length chars) num ]
    in
    div
      ([ id <| "line " ++ String.fromInt num
      , css [ minHeight (px lineHeightConst), lineHeight (px <| lineHeightConst + 3) ]
      ] ++ lineEvents)
      <| viewLineNumber num
      ::  ( chars
            |> List.indexedMap (\i v -> (v, isPositionSelected selection { column = i, line = num }))
            |> List.map viewChar
            |> List.indexedMap (\i v -> (createCharAttributes (hasCaret && (i == caretPos || (i == 0 && caretPos == 0))) isSelectionInProgress <| CaretPosition i num) <| v )
            |> insertOnMaybeIndex maybeCaretPos caretWrapper
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
      [ css [ height (pct 100)
            , width (px 50)
            , backgroundColor (rgb 0 0 200)
            , color (rgb 250 250 250)
            , marginRight (px 10)
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

errored word =
  span
    [ css [ textDecoration3 underline wavy <| rgb 250 0 0 ] ]
    [ word ]