module Section.FileUpload exposing (view)

import Html exposing (Html, button, div, h3, input, label, span, text)
import Html.Attributes exposing (attribute, class, classList, for, id, name, type_)
import Html.Events exposing (on)
import Json.Decode exposing (succeed)


view : Bool -> msg -> Html msg
view fileExists changeMsg =
    div [ class "" ]
        [ h3
            [ class "uk-heading-line uk-text-center" ]
            [ span [] [ text "Select a file" ]
            ]
        , input
            [ type_ "file", id "csv-upload", name "csv-upload", on "change" (succeed changeMsg) ]
            []
        , button
            [ class "file-upload-button uk-button uk-button-default uk-width-1-1"
            , classList [ ( "no-file", not fileExists ) ]
            ]
            [ label
                [ attribute "uk-icon" "icon: upload"
                , for "csv-upload"
                , class "file-label"
                ]
                [ text "" ]
            ]
        ]
