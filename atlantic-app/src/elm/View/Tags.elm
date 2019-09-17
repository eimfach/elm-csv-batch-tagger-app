module View.Tags exposing (viewList, viewTagButton, viewTagButtons, viewTagCloud)

import Data.Alias
import Data.Button
import Html exposing (Html, li, p, span, text, ul)
import Html.Attributes exposing (class)
import Set exposing (Set)
import View.Button as Button


viewList : (Data.Alias.Tag -> msg) -> Set Data.Alias.Tag -> Html msg
viewList msg tags =
    ul [ class "uk-list uk-list-divider" ]
        (tags
            |> Set.toList
            |> List.map (viewListItem msg)
        )


viewListItem : (Data.Alias.Tag -> msg) -> Data.Alias.Tag -> Html msg
viewListItem msg tag =
    li []
        [ span
            []
            [ text tag ]
        , Button.view
            (msg tag)
            (Data.Button.Composed Data.Button.Default Data.Button.Medium Data.Button.Right)
            Data.Button.Remove
            ""
        ]


viewTagButtons : (Data.Alias.Tag -> msg) -> Set Data.Alias.Tag -> List (Html msg)
viewTagButtons msg tags =
    tags
        |> Set.toList
        |> List.map (viewTagButton msg)


viewTagButton : (Data.Alias.Tag -> msg) -> Data.Alias.Tag -> Html msg
viewTagButton msg tag =
    Button.view
        (msg tag)
        (Data.Button.Composed Data.Button.Primary Data.Button.Medium Data.Button.NoAlign)
        Data.Button.NoActionType
        tag


viewTagCloud : (Data.Alias.Tag -> msg) -> Set Data.Alias.Tag -> Html msg
viewTagCloud action tags =
    p
        [ class "tag-cloud" ]
        (viewTagButtons
            action
            tags
        )
