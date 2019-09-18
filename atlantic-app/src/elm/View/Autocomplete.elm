module View.Autocomplete exposing (view)

import Html exposing (Html, datalist, div, input, label, option, text)
import Html.Attributes exposing (attribute, class, for, id, list)
import Set exposing (Set)
import View.Input as Input


view : String -> String -> String -> List (Html.Attribute msg) -> Set String -> Html msg
view labelText val idVal inputAttr options =
    div [ class "uk-form-horizontal" ]
        [ div [ class "uk-margin" ]
            [ label [ class "uk-form-label" ] [ text labelText ]
            , Input.viewDefault val
                (inputAttr
                    ++ [ list idVal ]
                )
            , viewDataList idVal options
            ]
        ]


viewDataList : String -> Set String -> Html msg
viewDataList idVal options =
    datalist
        [ id idVal ]
        (options
            |> Set.toList
            |> List.map viewListItem
        )


viewListItem : String -> Html msg
viewListItem content =
    option [] [ text content ]
