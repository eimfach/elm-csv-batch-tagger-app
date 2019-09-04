port module Ports.FileReader exposing (FileData, decodeFileContents, decodeFileData, encodeFileData, fileContentRead, fileSelected)

import Base64
import Json.Decode as Decode
import Json.Encode as Encode


type alias FileData =
    { contents : String
    , filename : String
    }


decodeFileContents : String -> Result String String
decodeFileContents encodedData =
    encodedData
        |> Base64.decode


encodeFileData : Maybe FileData -> Encode.Value
encodeFileData fileData =
    case fileData of
        Just fileData_ ->
            Encode.object
                [ ( "contents", Encode.string fileData_.contents )
                , ( "filename", Encode.string fileData_.filename )
                ]

        Nothing ->
            Encode.null


decodeFileData : Decode.Value -> String -> Maybe FileData
decodeFileData decodedValue fieldName =
    let
        content =
            Decode.decodeValue (Decode.field fieldName (Decode.field "contents" Decode.string)) decodedValue

        filename =
            Decode.decodeValue (Decode.field fieldName (Decode.field "filename" Decode.string)) decodedValue
    in
    case ( content, filename ) of
        ( Ok content_, Ok filename_ ) ->
            Just <| FileData content_ filename_

        _ ->
            Nothing


port fileSelected : String -> Cmd msg


port fileContentRead : (FileData -> msg) -> Sub msg
