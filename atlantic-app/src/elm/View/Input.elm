module View.Input exposing (viewDefault, viewWithButton, viewRadio, viewRadioGroup)

import Data.Button
import Data.Input
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (class, classList, name, type_)
import Html.Events exposing (onClick)
import View.Button as Button


view : List (Html msg) -> Html msg
view childs =
    div [ class "input-group" ]
        childs


viewDefault : Data.Input.Attributes msg -> Html msg
viewDefault inputAttr =
    view
        [ input
            ([ class "uk-input", type_ "text" ]
                ++ inputAttr
            )
            []
        ]


viewWithButton :
    Data.Input.Attributes msg
    -> Data.Button.ActionType
    -> msg
    -> Html msg
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
                Data.Button.Default
                btnActionType
                ""
            ]
        ]


{-|
    message
  , groupName
  , labelText
-}
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
