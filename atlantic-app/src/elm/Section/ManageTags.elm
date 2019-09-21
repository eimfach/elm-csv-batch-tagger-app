module Section.ManageTags exposing (view)

import Data.Alias
import Data.Button
import Html exposing (div, h3, span, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import Set exposing (Set)
import View.Input
import View.Tags


view : String -> String -> ( String, Bool ) -> String -> Set Data.Alias.Tag -> (Data.Alias.Tag -> msg) -> msg -> (Data.Alias.Tag -> msg) -> Html.Html msg
view headerText inputPlaceholder error buffer tags tagInputMsg createTagMsg removeTagMsg =
    div []
        [ div []
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span [ class "uk-text-background uk-text-large" ] [ text headerText ]
                ]
            ]
        , div [ class "uk-padding" ]
            [ View.Input.viewWithButton
                [ onInput tagInputMsg, value buffer, placeholder inputPlaceholder ]
                Data.Button.Add
                createTagMsg
            , View.Tags.viewList
                (\tag -> removeTagMsg tag)
                tags
            ]
        ]
