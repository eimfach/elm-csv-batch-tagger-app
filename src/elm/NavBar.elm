module NavBar exposing (NavItem(..), viewIconNav)

import Helpers
import Html exposing (button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)
import Locale exposing (Locale)



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


mapActionToElement : Locale -> ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
mapActionToElement locale ( action, msg, attr ) =
    case action of
        CountBadge val ->
            span [ class "uk-badge uk-padding-small" ] [ text <| String.fromInt val ++ " " ++ Locale.translateResults locale ]

        Spacer ->
            viewIconNavItem "" [ style "border-left" "1px solid #efefef", style "height" "26px" ] msg

        Workspace ->
            viewIconNavItem (Locale.translateSwitchWorkspace locale) (attribute "uk-icon" "icon: folder" :: attr) msg

        TagData ->
            viewIconNavItem (Locale.translateTagThisData locale) (attribute "uk-icon" "icon: tag" :: attr) msg

        Language ->
            viewIconNavItem (Locale.translateToggleLanguage locale) (attribute "uk-icon" "icon: world" :: attr) msg

        Delete ->
            viewIconNavItem (Locale.translateDeleteWorkspace locale) (attribute "uk-icon" "icon: trash" :: attr) msg

        ViewTaggedData ->
            viewIconNavItem (Locale.translateViewTaggedData locale) (attribute "uk-icon" "icon: database" :: attr) msg

        ViewManageTags ->
            viewIconNavItem (Locale.translateManageYourTags locale) (attribute "uk-icon" "icon: tag" :: attr) msg

        Undo ->
            viewIconNavItem (Locale.translateUndo locale) (attribute "uk-icon" "icon: reply" :: attr) msg

        Redo ->
            viewIconNavItem (Locale.translateRedo locale) (attribute "uk-icon" "icon: forward" :: attr) msg

        Forward ->
            viewIconNavItem "Forwards" (attribute "uk-icon" "icon: chevron-right" :: attr) msg

        Backward ->
            viewIconNavItem "Backwards" (attribute "uk-icon" "icon: chevron-left" :: attr) msg

        Export ->
            viewIconNavItem (Locale.translateExportToCSV locale) (attribute "uk-icon" "icon: download" :: attr) msg

        Import ->
            viewIconNavItem (Locale.translateImportFromCSV locale) (attribute "uk-icon" "icon: plus" :: attr) msg

        ViewTableData ->
            viewIconNavItem (Locale.translateViewWorkingData locale) (attribute "uk-icon" "icon: table" :: attr) msg

        Disabled action_ ->
            mapActionToElement locale ( action_, msg, [ class "ui-disabled uk-disabled" ] )


viewIconNav : Locale -> Helpers.Padding -> List ( NavItem, msg, List (Html.Attribute msg) ) -> List ( NavItem, msg, List (Html.Attribute msg) ) -> Html.Html msg
viewIconNav locale activePadding destructiveButtons constructiveButtons =
    div
        [ class <| "uk-flex uk-flex-between " ++ Helpers.getPaddingClasses activePadding ]
        [ ul [ class "uk-iconnav" ]
            (List.map
                (mapActionToElement locale)
                destructiveButtons
            )
        , ul [ class "uk-iconnav" ]
            (List.map
                (mapActionToElement locale)
                constructiveButtons
            )
        ]
