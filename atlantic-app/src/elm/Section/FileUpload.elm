module Section.FileUpload exposing (view)

import Html exposing (Html, button, div, h3, input, label, span, text)
import Html.Attributes exposing (attribute, class, classList, for, id, name, type_)
import Html.Events exposing (on)
import Json.Decode exposing (succeed)


view : String -> Bool -> msg -> Html msg
view headerText fileExists changeMsg =
    div [ class "uk-section uk-section-small" ]
        [ h3
            [ class "uk-heading-line uk-text-center" ]
            [ span [ class "uk-text-background uk-text-large" ] [ text headerText ]
            ]
        , div
            [ class "uk-padding" ]
            [ input
                [ type_ "file", id "csv-upload", name "csv-upload", on "change" (succeed changeMsg) ]
                []
            , button
                [ class "file-upload-button uk-button uk-button-default uk-width-1-1 uk-margin"
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
        ]
