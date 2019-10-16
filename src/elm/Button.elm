module Button exposing (ActionType(..), Alignment(..), Size(..), Text, Type(..), view)

import Html exposing (button, span, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import Set


type ActionType
    = Add
    | Remove
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
    | Composed Type Size Alignment


type alias Text =
    String


mapButtonStyles : Type -> String
mapButtonStyles btnType =
    case btnType of
        Primary ->
            createPrefixedClass (Set.fromList [ "button", "button-primary" ])

        Secondary ->
            createPrefixedClass (Set.fromList [ "button", "button-secondary" ])

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
        Add ->
            [ attribute "uk-icon" "icon: plus-circle" ]

        Remove ->
            [ attribute "uk-icon" "icon: minus-circle" ]

        NoActionType ->
            []


view :
    msg
    -> Type
    -> ActionType
    -> Text
    -> Html.Html msg
view msg btnType btnActionType btnText =
    button
        [ type_ "button", onClick msg, class (mapButtonStyles btnType) ]
        [ span
            (mapButtonActionType btnActionType)
            [ text btnText
            ]
        ]


createPrefixedClass : Set.Set String -> String
createPrefixedClass classes =
    let
        prefix =
            "uk-"
    in
    Set.foldr (\class acc -> acc ++ prefix ++ class ++ " ") "" classes
