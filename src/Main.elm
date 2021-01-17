module Main exposing (..)
import Array exposing (Array)
import Browser
import Browser.Dom as Dom exposing (Element)
import Css exposing (..)
import Css.Animations as Animation exposing (keyframes)
import Html.Styled.Events exposing (onClick, onInput, preventDefaultOn)
import Html.Styled exposing (Html, br, div, span, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Json.Decode as Json
import KeyboardMsg exposing (KeyboardMsg(..), keyboardMsgDecoder)
import Parser exposing ((|=), Parser)
import String
import Task as Task

main : Program () Model Msg
main =
    Browser.element
      { init = always ({ textValue = "empty", caretPosition = 0, highlighter = testParser }, Cmd.none)
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


lineHeightConst = 15
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
      | ClickChar Int
      | None
      | DebugFail String

type alias Model =
  { textValue : String
  , caretPosition : Int
  , highlighter : Parser (List StyledChar)
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
        ClickChar i -> ({ model | caretPosition = i }, Cmd.none)
        KeyboardMsgWrapper keyMsg -> (updateAfterKeyboardMsg keyMsg model, scrollToCaretIfNeeded)
        None -> (model, Cmd.none)
        DebugFail error -> ({ model | textValue = error }, Cmd.none)

updateAfterKeyboardMsg : KeyboardMsg -> Model -> Model
updateAfterKeyboardMsg msg model =
  case msg of
    InsertChar c -> { model | textValue = insertCharAt model.caretPosition c model.textValue, caretPosition = model.caretPosition + 1 }
    MoveCaretRight -> { model | caretPosition = updateCaretPos ((+) 1) model.textValue model.caretPosition }
    MoveCaretLeft -> { model | caretPosition = updateCaretPos (\x -> x - 1) model.textValue model.caretPosition }
    MoveCaretUp -> Debug.log "not implemented yet" model
    MoveCaretDown -> Debug.log "not implemented yet" model
    MoveCaretToLineEnd -> Debug.log "not implemented yet" model
    MoveCaretToLineStart -> Debug.log "not implemented yet" model
    MoveCaretToTheStart -> { model | caretPosition = 0 }
    MoveCaretToTheEnd -> { model | caretPosition = String.length model.textValue }
    RemoveNextChar -> { model | textValue = removeCharAt model.caretPosition model.textValue }
    RemovePrevChar -> { model | textValue = removeCharAt (model.caretPosition - 1) model.textValue, caretPosition = updateCaretPos (\x -> x - 1) model.textValue model.caretPosition }

updateCaretPos : (Int -> Int) -> String -> Int -> Int
updateCaretPos updater str oldPos =
  let
    newPos = updater oldPos
  in
  if newPos > (String.length str) || newPos < 0
   then oldPos
   else newPos

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
    if Debug.log "Should move left: " <| shouldMoveLeft el
      then el.element.x - viewportCaretPadding - viewportOverMoveToCaretMargin + caretWidth
      else el.element.x - el.viewport.width + viewportCaretPadding + viewportOverMoveToCaretMargin

shouldMove : Element -> Bool
shouldMove el =
    shouldMoveLeft el || shouldMoveRight el

moveViewportIfNecessary : Element -> Task.Task Dom.Error ()
moveViewportIfNecessary el =
    if Debug.log "Should move: " <| shouldMove <| Debug.log "Viewport result: " el
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
    [ css [ padding (px paddingSize), overflow hidden, boxSizing borderBox ] ]
    [ div
        [ css
            [ whiteSpace noWrap
            , fontSize (px fontSizeConst)
            , maxWidth (px lineWidthConst)
            , lineHeight (px lineHeightConst)
            , position relative
            , focus [ backgroundColor (rgb 10 200 50) ]
            , overflowX auto
            , outline none
            ]
          , preventDefaultOn "keydown" <| wrapKeyboardDecoder keyboardMsgDecoder
          , tabindex 0
        , id "editor"
        ]
        ( Parser.run model.highlighter model.textValue
            |> Result.withDefault []
            |> List.map viewChar
            |> insertOnIndex model.caretPosition caretWrapper
            |> List.indexedMap (\i v -> span [ onClick (ClickChar i), id (if i == model.caretPosition - 1 || (i == 0 && model.caretPosition == 0) then "caretChar" else "") ] [v])
        )
    ]

viewChar : StyledChar -> Html Msg
viewChar char =
    let
      renderedChar = text <| String.fromChar char.value
    in
    case (char.isKeyword, char.isErrored) of
      (True, True) -> keyword <| errored renderedChar
      (True, False) -> keyword renderedChar
      (False, True) -> errored renderedChar
      (False, False) -> renderedChar

insertOnIndex : Int -> a -> List a -> List a
insertOnIndex index element lst =
  List.take index lst ++ element :: List.drop index lst

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

errored word =
  span
    [ css [ textDecoration3 underline wavy <| rgb 250 0 0 ] ]
    [ word ]