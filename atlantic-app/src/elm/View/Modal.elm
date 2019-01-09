module View.Modal exposing (view)

import Data.Button
import Data.Modal
import Html exposing (button, div, h2, p, text)
import Html.Attributes exposing (attribute, class, classList, id, style, type_)
import View.Button


viewModalButton : Data.Modal.Button msg -> Html.Html msg
viewModalButton ( button, msg, descr ) =
    View.Button.view msg button Data.Button.NoActionType descr


view : Data.Modal.Visibility -> String -> Html.Html msg -> List (Data.Modal.Button msg) -> Html.Html msg
view visiblity heading content buttons =
    let
        ( active, css ) =
            case visiblity of
                Data.Modal.Visible ->
                    ( True, style [ ( "display", "block" ) ] )

                Data.Modal.NotVisible ->
                    ( False, style [ ( "", "" ) ] )
    in
        div [ class "uk-modal", classList [ ( "uk-open", active ) ], css ]
            [ div [ class "uk-modal-dialog", attribute "uk-overflow-auto" "" ]
                [ button [ class "uk-modal-close-default", type_ "button", attribute "uk-close" "" ]
                    []
                , div [ class "uk-modal-header" ]
                    [ h2 [ class "uk-modal-title" ]
                        [ text heading ]
                    ]
                , div [ class "uk-modal-body" ]
                    [ content ]
                , div
                    [ class "uk-modal-footer uk-text-right" ]
                    (List.map viewModalButton buttons)
                ]
            ]
