module View.Autocomplete exposing (view)

import Html exposing (Html, datalist, div, input, option, text)
import Html.Attributes exposing (attribute, class, id, list)
import Set exposing (Set)
import View.Input as Input


view : String -> String -> List (Html.Attribute msg) -> Set String -> Html msg
view labelText idVal inputAttr options =
    div []
        [ text labelText
        , Input.viewDefault
            (inputAttr
                ++ [ list idVal ]
            )
        , viewDataList idVal options
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
