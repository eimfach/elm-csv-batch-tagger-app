module View.Modal exposing (view)

import Data.Button
import Data.Modal
import Html exposing (button, div, h2, p, text)
import Html.Attributes exposing (attribute, class, classList, id, style, type_)
import Html.Events exposing (onClick)
import View.Button


viewModalButton : Data.Modal.Button msg -> Html.Html msg
viewModalButton ( button, msg, descr ) =
    View.Button.view msg button Data.Button.NoActionType descr


view : Data.Modal.DisplayProperties -> msg -> Data.Modal.Visibility -> String -> Html.Html msg -> List (Data.Modal.Button msg) -> Html.Html msg
view displayProperties closeMsg visiblity heading content buttons =
    let
        ( isOpen, css ) =
            case visiblity of
                Data.Modal.Visible ->
                    ( True, style "display" "block" )

                Data.Modal.NotVisible ->
                    ( False, style "" "" )

        displayPropertyClasses =
            case displayProperties of
                Data.Modal.RegularView ->
                    [ ( "uk-modal-full", False ) ]

                Data.Modal.Fullscreen ->
                    [ ( "uk-modal-full", True ) ]
    in
    div [ class "uk-modal", class "uk-modal-container", classList <| [ ( "uk-open", isOpen ) ] ++ displayPropertyClasses, css ]
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
