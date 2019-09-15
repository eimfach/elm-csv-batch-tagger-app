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


view : Bool -> msg -> Data.Modal.Visibility -> String -> Html.Html msg -> List (Data.Modal.Button msg) -> Html.Html msg
view showFull closeMsg visiblity heading content buttons =
    let
        ( active, css ) =
            case visiblity of
                Data.Modal.Visible ->
                    ( True, style "display" "block" )

                Data.Modal.NotVisible ->
                    ( False, style "" "" )
    in
    div [ class "uk-modal", class "uk-modal-container", classList [ ( "uk-open", active ), ( "uk-modal-full", showFull ) ], css ]
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
