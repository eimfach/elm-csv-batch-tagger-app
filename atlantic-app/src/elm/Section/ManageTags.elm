module Section.ManageTags exposing (view)

import Data.Alias
import Data.Button
import Html exposing (div, h3, span, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)
import View.Input
import View.Tags


view : ( String, Bool ) -> String -> Set Data.Alias.Tag -> (Data.Alias.Tag -> msg) -> msg -> (Data.Alias.Tag -> msg) -> Html.Html msg
view error buffer tags tagInputMsg createTagMsg removeTagMsg =
    div [ class "" ]
        [ div [ class "" ]
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span [] [ text "Manage your tags" ]
                ]
            ]
        , div [ class "" ]
            [ View.Input.viewWithButton
                [ onInput tagInputMsg, value buffer, placeholder "enter a tag" ]
                Data.Button.Add
                createTagMsg
            , View.Tags.viewList
                (\tag -> removeTagMsg tag)
                tags
            ]
        ]
