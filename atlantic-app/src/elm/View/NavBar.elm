module View.NavBar exposing (NavItem(..), view, viewIconNav)

import Html exposing (a, div, li, nav, text, ul)
import Html.Attributes exposing (attribute, class, href)
import Html.Events exposing (onClick)



--deprecated--


type NavItem
    = Undo
    | Redo
    | Forward
    | Backward
    | Export
    | Disabled NavItem


viewIconNavItem : List (Html.Attribute a) -> a -> Html.Html a
viewIconNavItem attr msg =
    li []
        [ a (attr ++ [ onClick msg, href "javascript:void(0);", class "uk-icon" ])
            []
        ]


mapActionToElement : ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
mapActionToElement ( action, msg, attr ) =
    case action of
        Undo ->
            viewIconNavItem ([ attribute "uk-icon" "icon: reply" ] ++ attr) msg

        Redo ->
            viewIconNavItem ([ attribute "uk-icon" "icon: forward" ] ++ attr) msg

        Forward ->
            viewIconNavItem ([ attribute "uk-icon" "icon: chevron-right" ] ++ attr) msg

        Backward ->
            viewIconNavItem ([ attribute "uk-icon" "icon: chevron-left" ] ++ attr) msg

        Export ->
            viewIconNavItem ([ attribute "uk-icon" "icon: download" ] ++ attr) msg

        Disabled action_ ->
            mapActionToElement ( action_, msg, [ class "ui-disabled uk-disabled" ] )


view : Html.Html msg
view =
    nav [ class "uk-navbar-container", attribute "uk-navbar" "" ]
        [ div [ class "uk-navbar-left" ]
            [ ul [ class "uk-navbar-nav" ]
                [ li [ class "uk-active" ]
                    [ a [ href "" ]
                        []
                    ]
                , li [ class "uk-parent" ]
                    [ a [ href "" ]
                        [ text "Test" ]
                    ]
                , li []
                    [ a [ href "" ]
                        [ text "Test" ]
                    ]
                ]
            ]
        ]


viewIconNav : List ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
viewIconNav buttonList =
    div
        [ class "uk-position-top-right icon-nav" ]
        [ ul [ class "uk-iconnav" ]
            (List.map
                mapActionToElement
                buttonList
            )
        ]
