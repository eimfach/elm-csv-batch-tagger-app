module Button exposing (ActionType(..), Alignment(..), Size(..), Text, Type(..), view, viewWithDropDown)

import Dict exposing (Dict)
import Html exposing (button, div, h3, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, type_)
import Html.Events exposing (onClick)
import Set
import StoryBook.SortTable exposing (Msg)


type ActionType
    = Add
    | Remove
    | Import
    | AddRegex
    | NoActionType


type Size
    = Small
    | Medium
    | Large


type Alignment
    = Left
    | Right
    | NoAlign


type Type
    = Primary
    | Secondary
    | Default
    | Danger
    | Composed Type Size Alignment


type alias Text =
    String


mapButtonStyles : Type -> String
mapButtonStyles btnType =
    case btnType of
        Danger ->
            createPrefixedClass (Set.fromList [ "button", "button-danger" ])

        Primary ->
            createPrefixedClass (Set.fromList [ "button", "button-primary" ])

        Secondary ->
            createPrefixedClass (Set.fromList [ "button", "button-default" ])

        Default ->
            createPrefixedClass (Set.fromList [ "button", "button-default" ])

        Composed btnType_ sizeTag alignment ->
            let
                sizeClasses =
                    case sizeTag of
                        Small ->
                            createPrefixedClass (Set.fromList [ "button-small" ])

                        Medium ->
                            createPrefixedClass (Set.fromList [ "button-medium" ])

                        Large ->
                            createPrefixedClass (Set.fromList [ "button-large" ])

                alignmentClasses =
                    case alignment of
                        Left ->
                            createPrefixedClass (Set.fromList [ "align-left" ])

                        Right ->
                            createPrefixedClass (Set.fromList [ "align-right" ])

                        NoAlign ->
                            createPrefixedClass Set.empty
            in
            mapButtonStyles btnType_ ++ sizeClasses ++ alignmentClasses


mapButtonActionType : ActionType -> List (Html.Attribute msg)
mapButtonActionType btnActionType =
    case btnActionType of
        Import ->
            [ attribute "uk-icon" "icon: pull" ]

        Add ->
            [ attribute "uk-icon" "icon: plus-circle" ]

        Remove ->
            [ attribute "uk-icon" "icon: minus-circle" ]

        NoActionType ->
            []

        AddRegex ->
            [ attribute "uk-icon" "icon: nut" ]


view :
    msg
    -> Type
    -> ActionType
    -> Text
    -> Html.Html msg
view msg btnType btnActionType btnText =
    button
        [ type_ "button", onClick msg, class (mapButtonStyles btnType) ]
        [ span (mapButtonActionType btnActionType) []
        , span [] [ text <| "  " ++ btnText ]
        ]


viewWithDropDown : Type -> ActionType -> Text -> Dict String (List ( String, msg )) -> Html.Html msg
viewWithDropDown btnType btnActionType btnText dropDownList =
    div []
        [ button
            [ type_ "button", class (mapButtonStyles btnType) ]
            [ span (mapButtonActionType btnActionType) []
            ]
        , div [ class "uk-width-2xlarge", attribute "uk-dropdown" "mode: click; pos: bottom-center" ]
            [ h3 [] [ text btnText ]
            , div [ class "uk-dropdown-grid uk-child-width-1-2@m", attribute "uk-grid" "" ] <|
                viewGridDropdownList dropDownList
            ]
        ]


viewGridDropdownList : Dict String (List ( String, msg )) -> List (Html.Html msg)
viewGridDropdownList dropDownList =
    Dict.map
        (\title options ->
            div []
                [ ul [ class "uk-nav uk-dropdown-nav" ] <|
                    [ li [ class "uk-nav-header" ] [ text title ] ]
                        ++ List.map (\( descr, action ) -> li [ onClick action ] [ text descr ]) options
                ]
        )
        dropDownList
        |> Dict.values


createPrefixedClass : Set.Set String -> String
createPrefixedClass classes =
    let
        prefix =
            "uk-"
    in
    Set.foldr (\class acc -> acc ++ prefix ++ class ++ " ") "" classes
