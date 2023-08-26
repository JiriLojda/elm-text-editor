module KeyboardMsg exposing (KeyboardMsg(..), keyboardMsgDecoder)

import Basics.Extra exposing (flip)
import Json.Decode as Json
import List.Extra as ListE
import Tuple exposing (pair)


type KeyboardMsg
    = InsertChar Char
    | MoveCaretLeft
    | MoveCaretRight
    | MoveCaretUp
    | MoveCaretDown
    | MoveCaretLeftWithSelection
    | MoveCaretRightWithSelection
    | MoveCaretUpWithSelection
    | MoveCaretDownWithSelection
    | MoveCaretToLineEnd
    | MoveCaretToLineStart
    | MoveCaretToTheStart
    | MoveCaretToTheEnd
    | MoveCaretToLineEndWithSelection
    | MoveCaretToLineStartWithSelection
    | MoveCaretToTheStartWithSelection
    | MoveCaretToTheEndWithSelection
    | RemovePrevChar
    | RemoveNextChar
    | AddNewLine
    | Copy
    | Paste
    | Undo
    | Redo


keyboardMsgDecoder : Json.Decoder KeyboardMsg
keyboardMsgDecoder =
    Json.map2 pair keyDecoder modifiersDecoder
        |> Json.andThen keyMsgDecoder


type Key
    = CharKey Char
    | ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | Home
    | End
    | Backspace
    | Delete
    | Enter


type Modifier
    = CtrlKey
    | ShiftKey
    | AltKey
    | ComandKey


type alias Modifiers =
    List Modifier


keyDecoder : Json.Decoder Key
keyDecoder =
    Json.field "key" Json.string
        |> Json.andThen
            (\key ->
                let
                    char =
                        case String.uncons key of
                            Just ( c, "" ) ->
                                Just c

                            _ ->
                                Nothing
                in
                case ( char, key ) of
                    ( Just c, _ ) ->
                        Json.succeed (CharKey c)

                    ( _, "ArrowUp" ) ->
                        Json.succeed ArrowUp

                    ( _, "ArrowDown" ) ->
                        Json.succeed ArrowDown

                    ( _, "ArrowLeft" ) ->
                        Json.succeed ArrowLeft

                    ( _, "ArrowRight" ) ->
                        Json.succeed ArrowRight

                    ( _, "Home" ) ->
                        Json.succeed Home

                    ( _, "End" ) ->
                        Json.succeed End

                    ( _, "Backspace" ) ->
                        Json.succeed Backspace

                    ( _, "Delete" ) ->
                        Json.succeed Delete

                    ( _, "Enter" ) ->
                        Json.succeed Enter

                    ( _, str ) ->
                        Json.fail <| "Not supported key: '" ++ str ++ "'. (at least for now)"
            )


modifiersDecoder : Json.Decoder Modifiers
modifiersDecoder =
    let
        parseModifier : Modifier -> String -> Json.Decoder (Maybe Modifier)
        parseModifier modifier name =
            Json.map
                (\x ->
                    if x then
                        Just modifier

                    else
                        Nothing
                )
                (Json.field name Json.bool)

        addJust : Maybe a -> List a -> List a
        addJust m lst =
            case m of
                Just x ->
                    x :: lst

                Nothing ->
                    lst

        foldModifierDecoders : List (Json.Decoder (Maybe Modifier)) -> Json.Decoder (List Modifier)
        foldModifierDecoders =
            List.foldl (Json.map2 addJust) (Json.succeed [])
    in
    foldModifierDecoders
        [ parseModifier CtrlKey "ctrlKey"
        , parseModifier AltKey "altKey"
        , parseModifier ShiftKey "shiftKey"
        , parseModifier ComandKey "metaKey"
        ]


excludes : List a -> List a -> Bool
excludes shouldBeExcluded set =
    List.all (not << flip List.member set) shouldBeExcluded


succeedIf : String -> Bool -> a -> Json.Decoder a
succeedIf failMsg condition result =
    if condition then
        Json.succeed result

    else
        Json.fail failMsg


succeedMsgIf : Bool -> a -> Json.Decoder a
succeedMsgIf =
    succeedIf "Unsupported keyboard shortcut"


keyMsgDecoder : ( Key, Modifiers ) -> Json.Decoder KeyboardMsg
keyMsgDecoder ( key, modifiers ) =
    case key of
        CharKey c ->
            Json.oneOf
                [ succeedMsgIf (c == 'y' && modifiers == [ CtrlKey ] || modifiers == [ ComandKey ]) Redo
                , succeedMsgIf (c == 'z' && modifiers == [ CtrlKey ] || modifiers == [ ComandKey ]) Undo
                , succeedMsgIf (c == 'v' && modifiers == [ CtrlKey ] || modifiers == [ ComandKey ]) Paste
                , succeedMsgIf (c == 'c' && modifiers == [ CtrlKey ] || modifiers == [ ComandKey ]) Copy
                , succeedMsgIf (excludes [ CtrlKey, AltKey, ComandKey ] modifiers) (InsertChar c)
                ]

        ArrowLeft ->
            Json.oneOf
                [ succeedMsgIf (modifiers == []) MoveCaretLeft
                , succeedMsgIf (modifiers == [ ShiftKey ]) MoveCaretLeftWithSelection
                , succeedMsgIf (modifiers == [ ComandKey ]) MoveCaretToLineStart
                , succeedMsgIf (ListE.isPermutationOf [ ComandKey, ShiftKey ] modifiers) MoveCaretToLineStartWithSelection
                ]

        ArrowRight ->
            Json.oneOf
                [ succeedMsgIf (modifiers == []) MoveCaretRight
                , succeedMsgIf (modifiers == [ ShiftKey ]) MoveCaretRightWithSelection
                , succeedMsgIf (modifiers == [ ComandKey ]) MoveCaretToLineEnd
                , succeedMsgIf (ListE.isPermutationOf [ ComandKey, ShiftKey ] modifiers) MoveCaretToLineEndWithSelection
                ]

        ArrowUp ->
            Json.oneOf
                [ succeedMsgIf (modifiers == []) MoveCaretUp
                , succeedMsgIf (modifiers == [ ShiftKey ]) MoveCaretUpWithSelection
                , succeedMsgIf (modifiers == [ ComandKey ]) MoveCaretToTheStart
                , succeedMsgIf (ListE.isPermutationOf [ ComandKey, ShiftKey ] modifiers) MoveCaretToTheStartWithSelection
                ]

        ArrowDown ->
            Json.oneOf
                [ succeedMsgIf (modifiers == []) MoveCaretDown
                , succeedMsgIf (modifiers == [ ShiftKey ]) MoveCaretDownWithSelection
                , succeedMsgIf (modifiers == [ ComandKey ]) MoveCaretToTheEnd
                , succeedMsgIf (ListE.isPermutationOf [ ComandKey, ShiftKey ] modifiers) MoveCaretToTheEndWithSelection
                ]

        End ->
            Json.oneOf
                [ succeedMsgIf (modifiers == []) MoveCaretToLineEnd
                , succeedMsgIf (modifiers == [ CtrlKey ]) MoveCaretToTheEnd
                , succeedMsgIf (modifiers == [ ShiftKey ]) MoveCaretToLineEndWithSelection
                , succeedMsgIf (ListE.isPermutationOf [ ShiftKey, CtrlKey ] modifiers) MoveCaretToTheEndWithSelection
                ]

        Home ->
            Json.oneOf
                [ succeedMsgIf (modifiers == []) MoveCaretToLineStart
                , succeedMsgIf (modifiers == [ CtrlKey ]) MoveCaretToTheStart
                , succeedMsgIf (modifiers == [ ShiftKey ]) MoveCaretToLineStartWithSelection
                , succeedMsgIf (ListE.isPermutationOf [ ShiftKey, CtrlKey ] modifiers) MoveCaretToTheStartWithSelection
                ]

        Delete ->
            succeedMsgIf (modifiers == []) RemoveNextChar

        Backspace ->
            succeedMsgIf (modifiers == []) RemovePrevChar

        Enter ->
            succeedMsgIf (excludes [ CtrlKey, AltKey ] modifiers) AddNewLine
