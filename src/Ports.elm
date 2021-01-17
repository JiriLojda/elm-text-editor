port module Ports exposing (..)

port textClicked : String -> Cmd msg

port caretPositionChanged : (Int -> msg) -> Sub msg
