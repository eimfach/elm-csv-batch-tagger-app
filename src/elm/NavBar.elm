module NavBar exposing (NavItem(..), viewIconNav)

import Html exposing (button, div, li, ul)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)



--deprecated--


type NavItem
    = Undo
    | Redo
    | Forward
    | Backward
    | Export
    | ViewTableData
    | Disabled NavItem


viewIconNavItem : List (Html.Attribute a) -> a -> Html.Html a
viewIconNavItem attr msg =
    li []
        [ button (attr ++ [ onClick msg, class "uk-icon" ])
            []
        ]


mapActionToElement : ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
mapActionToElement ( action, msg, attr ) =
    case action of
        Undo ->
            viewIconNavItem (attribute "uk-icon" "icon: reply" :: attr) msg

        Redo ->
            viewIconNavItem (attribute "uk-icon" "icon: forward" :: attr) msg

        Forward ->
            viewIconNavItem (attribute "uk-icon" "icon: chevron-right" :: attr) msg

        Backward ->
            viewIconNavItem (attribute "uk-icon" "icon: chevron-left" :: attr) msg

        Export ->
            viewIconNavItem (attribute "uk-icon" "icon: download" :: attr) msg

        ViewTableData ->
            viewIconNavItem (attribute "uk-icon" "icon: table" :: attr) msg

        Disabled action_ ->
            mapActionToElement ( action_, msg, [ class "ui-disabled uk-disabled" ] )


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
