module Playground exposing (..)

import Browser
import Css exposing (height, marginLeft, px, width)
import Html.Styled exposing (Html, br, div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Parser exposing ((|.), (|=), Parser)
import Main as Editor exposing (defaultTheme)

main : Program () Editor.Model Editor.Msg
main =
    Browser.element
      { init = Editor.init testParser theme
      , update = Editor.update
      , view = view >> toUnstyled
      , subscriptions = always Sub.none
      }

textSize = 15

theme : Editor.Theme
theme =
  { defaultTheme
  | caretWidth = 1
  , caret = Editor.Styles [("background-color", "grey")]
  , root = Editor.Styles [("background-color", "#f2f2f2"), ("font-size", String.fromFloat textSize ++ "px")]
  , selection = Editor.Styles [("background-color", "rgb(224, 2, 179)")]
  , lineHeight = textSize
  }


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

parseToken : String -> String -> List Editor.StyledFragment -> Parser (Parser.Step (List Editor.StyledFragment) (List Editor.StyledFragment))
parseToken token class tmpResults =
  located (Parser.succeed class |. Parser.token token)
    |> Parser.map (\found -> Parser.Loop <| Editor.StyledFragment found.from class found.to :: tmpResults)

testParserStep : List Editor.StyledFragment -> Parser (Parser.Step (List Editor.StyledFragment) (List Editor.StyledFragment))
testParserStep results =
  Parser.oneOf
    [ parseToken "lol" "test-keyword" results
    , parseToken "lolz" "test-error" results
    , Parser.end |> Parser.map (always <| Parser.Done results)
    , Parser.chompIf (always True)
      |> Parser.map (always <| Parser.Loop results)
    ]

view : Editor.Model -> Html Editor.Msg
view model =
    div
      []
      [ br [] []
      --, viewTestArea model
      , div
        [ css
          [ height (px 300)
          , width (px 400)
          , marginLeft (px 50)
          ]
        ]
        [ Editor.view model
        ]
      ]

--viewTestArea model =
--  textarea
--    [ onInput TextChangedTestArea
--    , css [ width <| px 100, height <| px 100, backgroundColor <| rgb 100 200 0 ]
--    ]
--    [ text model.textValue
--    ]
