module Input exposing (viewAutocomplete, viewDefault, viewRadio, viewRadioGroup, viewWithButton)

import Button
import Html exposing (Attribute, Html, datalist, div, input, label, option, text)
import Html.Attributes exposing (class, id, list, name, type_, value)
import Html.Events exposing (onClick)
import Set exposing (Set)


view : List (Html msg) -> Html msg
view childs =
    div [ class "uk-form-controls" ]
        childs


viewDefault : String -> List (Attribute msg) -> Html msg
viewDefault val inputAttr =
    view
        [ input
            ([ class "uk-input", type_ "text", value val ]
                ++ inputAttr
            )
            []
        ]


viewWithButton : List (Attribute msg) -> Button.ActionType -> msg -> Html msg
viewWithButton inputAttr btnActionType msg =
    view
        [ div [ class "input-with-button" ]
            [ input
                ([ class "uk-input", type_ "text" ]
                    ++ inputAttr
                )
                []
            , Button.view
                msg
                Button.Default
                btnActionType
                ""
            ]
        ]


viewRadio : (String -> msg) -> String -> String -> Html msg
viewRadio action groupName labelText =
    div
        [ class "uk-width-1-1"
        ]
        [ label
            []
            [ input
                [ class "uk-radio"
                , type_ "radio"
                , name groupName
                , onClick (action labelText)
                ]
                []
            , text ("   " ++ labelText)
            ]
        ]


viewRadioGroup : String -> (String -> msg) -> List String -> List (Html msg)
viewRadioGroup groupName msg radioLabels =
    List.map (viewRadio msg groupName) radioLabels


viewAutocomplete : String -> String -> String -> List (Html.Attribute msg) -> Set String -> Html msg
viewAutocomplete labelText val idVal inputAttr options =
    div [ class "uk-form-horizontal" ]
        [ div [ class "uk-margin" ]
            [ label [ class "uk-form-label" ] [ text labelText ]
            , viewDefault val
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
