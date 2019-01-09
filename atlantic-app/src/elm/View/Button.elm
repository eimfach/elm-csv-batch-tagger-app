module View.Button exposing (view)

import Html exposing (button, span, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import Set
import Data.Button


mapButtonStyles : Data.Button.Type -> String
mapButtonStyles btnType =
    case btnType of
        Data.Button.Primary ->
            createPrefixedClass (Set.fromList [ "button", "button-primary" ])

        Data.Button.Secondary ->
            createPrefixedClass (Set.fromList [ "button", "button-secondary" ])

        Data.Button.Default ->
            createPrefixedClass (Set.fromList [ "button", "button-default" ])

        Data.Button.Composed btnType sizeTag alignment ->
            let
                sizeClasses =
                    case sizeTag of
                        Data.Button.Small ->
                            createPrefixedClass (Set.fromList [ "button-small" ])

                        Data.Button.Medium ->
                            createPrefixedClass (Set.fromList [ "button-medium" ])

                        Data.Button.Large ->
                            createPrefixedClass (Set.fromList [ "button-large" ])

                alignmentClasses =
                    case alignment of
                        Data.Button.Left ->
                            createPrefixedClass (Set.fromList [ "align-left" ])

                        Data.Button.Right ->
                            createPrefixedClass (Set.fromList [ "align-right" ])

                        Data.Button.NoAlign ->
                            createPrefixedClass Set.empty
            in
                (mapButtonStyles btnType) ++ sizeClasses ++ alignmentClasses


mapButtonActionType : Data.Button.ActionType -> List (Html.Attribute msg)
mapButtonActionType btnActionType =
    case btnActionType of
        Data.Button.Add ->
            [ attribute "uk-icon" "icon: plus-circle" ]

        Data.Button.Remove ->
            [ attribute "uk-icon" "icon: minus-circle" ]

        Data.Button.NoActionType ->
            []


view :
    msg
    -> Data.Button.Type
    -> Data.Button.ActionType
    -> Data.Button.Text
    -> Html.Html msg
view msg btnType btnActionType btnText =
    button
        ([ type_ "button", onClick msg ]
            ++ [ class (mapButtonStyles btnType) ]
        )
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
