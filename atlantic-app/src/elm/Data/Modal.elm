module Data.Modal exposing (Button, DisplayProperties(..), State, Title, Visibility(..), createStateDecoder, encodeState)

import Data.Button
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Button msg =
    ( Data.Button.Type, msg, Title )


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
