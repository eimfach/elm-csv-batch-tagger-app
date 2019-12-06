module Modal exposing (Button, DisplayProperties(..), State, Title, Visibility(..), createStateDecoder, encodeState, view)

import Button
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (attribute, class, classList, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Button msg =
    ( Button.Type, msg, Title )


type alias State content =
    { content : content
    , title : Title
    , displayProperties : DisplayProperties
    }


type alias Title =
    String


type Visibility
    = NotVisible
    | Visible


type DisplayProperties
    = RegularView
    | Fullscreen


encodeState : State content -> (content -> Encode.Value) -> Encode.Value
encodeState state contentEncoder =
    Encode.object
        [ ( "content", contentEncoder state.content )
        , ( "title", Encode.string state.title )
        , ( "displayProperties", encodeDisplayProperties state.displayProperties )
        ]


createStateDecoder : Decode.Decoder content -> Decode.Decoder (State content)
createStateDecoder contentDecoder =
    Decode.map3 State
        (Decode.field "content" contentDecoder)
        (Decode.field "title" Decode.string)
        (Decode.field "displayProperties" displayPropertiesDecoder)


encodeDisplayProperties : DisplayProperties -> Encode.Value
encodeDisplayProperties displayProps =
    case displayProps of
        RegularView ->
            Encode.string "regularView"

        Fullscreen ->
            Encode.string "fullscreen"


displayPropertiesDecoder : Decode.Decoder DisplayProperties
displayPropertiesDecoder =
    Decode.string |> Decode.andThen createDisplayPropertiesDecoder


createDisplayPropertiesDecoder : String -> Decode.Decoder DisplayProperties
createDisplayPropertiesDecoder encoded =
    case parseDisplayProperties encoded of
        Ok displayProperties ->
            Decode.succeed displayProperties

        Err err ->
            Decode.fail err


parseDisplayProperties : String -> Result String DisplayProperties
parseDisplayProperties encoded =
    case encoded of
        "regularView" ->
            Ok RegularView

        "fullscreen" ->
            Ok Fullscreen

        _ ->
            Err "Invalid DisplayProperties Encoding"


viewModalButton : Button msg -> Html msg
viewModalButton ( button, msg, descr ) =
    Button.view msg button Button.NoActionType descr


view : DisplayProperties -> msg -> String -> Html msg -> List (Button msg) -> Html msg
view displayProperties closeMsg heading content buttons =
    let
        displayPropertyClasses =
            case displayProperties of
                RegularView ->
                    [ ( "uk-modal-full", False ) ]

                Fullscreen ->
                    [ ( "uk-modal-full", True ) ]
    in
    div [ class "uk-modal", class "uk-modal-container uk-open", classList displayPropertyClasses, style "display" "block" ]
        [ div [ class "uk-modal-dialog" ]
            [ button [ class "uk-modal-close-full uk-close-large", type_ "button", attribute "uk-close" "", onClick closeMsg ]
                []
            , div [ class "uk-modal-header" ]
                [ h2 [ class "uk-modal-title" ]
                    [ text heading ]
                ]
            , div [ class "uk-modal-body", attribute "uk-overflow-auto" "" ]
                [ content ]
            , div
                [ class "uk-modal-footer uk-text-right" ]
                (List.map viewModalButton buttons)
            ]
        ]
