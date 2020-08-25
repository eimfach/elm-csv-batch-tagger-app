module NavBar exposing (NavItem(..), viewIconNav)

import Html exposing (button, div, li, ul)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)



--deprecated--


type NavItem
    = Undo
    | Redo
    | Forward
    | Backward
    | Export
    | Import
    | ViewTableData
    | ViewManageTags
    | ViewTaggedData
    | Spacer
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
        Spacer ->
            viewIconNavItem [ style "border-left" "1px solid #efefef", style "height" "26px" ] msg

        ViewTaggedData ->
            viewIconNavItem (attribute "uk-icon" "icon: database" :: attr) msg

        ViewManageTags ->
            viewIconNavItem (attribute "uk-icon" "icon: tag" :: attr) msg

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

        Import ->
            viewIconNavItem (attribute "uk-icon" "icon: plus" :: attr) msg

        ViewTableData ->
            viewIconNavItem (attribute "uk-icon" "icon: table" :: attr) msg

        Disabled action_ ->
            mapActionToElement ( action_, msg, [ class "ui-disabled uk-disabled" ] )


viewIconNav : List ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
viewIconNav buttonList =
    div
        [ class "uk-flex uk-flex-right uk-padding uk-padding-remove-top uk-padding-remove-bottom" ]
        [ ul [ class "uk-iconnav" ]
            (List.map
                mapActionToElement
                buttonList
            )
        ]
