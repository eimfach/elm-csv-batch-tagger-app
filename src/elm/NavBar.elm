module NavBar exposing (NavItem(..), viewIconNav)

import Html exposing (button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, style)
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
    | TagData
    | ViewTaggedData
    | Delete
    | Language
    | Workspace
    | Spacer
    | CountBadge Int
    | Disabled NavItem


viewIconNavItem : String -> List (Html.Attribute a) -> a -> Html.Html a
viewIconNavItem tooltip attr msg =
    li []
        [ button (attr ++ [ onClick msg, class "uk-icon", attribute "uk-tooltip" <| "title: " ++ tooltip ])
            []
        ]


mapActionToElement : ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
mapActionToElement ( action, msg, attr ) =
    case action of
        CountBadge val ->
            span [ class "uk-badge uk-padding-small" ] [ text <| String.fromInt val ++ " Results" ]

        Spacer ->
            viewIconNavItem "" [ style "border-left" "1px solid #efefef", style "height" "26px" ] msg

        Workspace ->
            viewIconNavItem "Switch Workspace" (attribute "uk-icon" "icon: folder" :: attr) msg

        TagData ->
            viewIconNavItem "Tag this Data" (attribute "uk-icon" "icon: tag" :: attr) msg

        Language ->
            viewIconNavItem "Toggle Language" (attribute "uk-icon" "icon: world" :: attr) msg

        Delete ->
            viewIconNavItem "Delete Workspace" (attribute "uk-icon" "icon: trash" :: attr) msg

        ViewTaggedData ->
            viewIconNavItem "View Tagged Data" (attribute "uk-icon" "icon: database" :: attr) msg

        ViewManageTags ->
            viewIconNavItem "Manage your Tags" (attribute "uk-icon" "icon: tag" :: attr) msg

        Undo ->
            viewIconNavItem "Undo" (attribute "uk-icon" "icon: reply" :: attr) msg

        Redo ->
            viewIconNavItem "Redo" (attribute "uk-icon" "icon: forward" :: attr) msg

        Forward ->
            viewIconNavItem "Forwards" (attribute "uk-icon" "icon: chevron-right" :: attr) msg

        Backward ->
            viewIconNavItem "Backwards" (attribute "uk-icon" "icon: chevron-left" :: attr) msg

        Export ->
            viewIconNavItem "Export to CSV" (attribute "uk-icon" "icon: download" :: attr) msg

        Import ->
            viewIconNavItem "Import from CSV" (attribute "uk-icon" "icon: plus" :: attr) msg

        ViewTableData ->
            viewIconNavItem "View Working Data" (attribute "uk-icon" "icon: table" :: attr) msg

        Disabled action_ ->
            mapActionToElement ( action_, msg, [ class "ui-disabled uk-disabled" ] )


viewIconNav : Bool -> List ( NavItem, msg, List (Html.Attribute msg) ) -> List ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
viewIconNav activePadding destructiveButtons constructiveButtons =
    div
        [ classList [ ( "uk-padding", activePadding ) ], class "uk-flex uk-flex-between uk-padding-remove-top uk-padding-remove-bottom" ]
        [ ul [ class "uk-iconnav" ]
            (List.map
                mapActionToElement
                destructiveButtons
            )
        , ul [ class "uk-iconnav" ]
            (List.map
                mapActionToElement
                constructiveButtons
            )
        ]
