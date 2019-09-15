module View.Table exposing (view, viewSingle, viewWithTagData)

import Data.Table exposing (flattenRows, prependCellToRow)
import Html exposing (Html, div, h3, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import View.NavBar as NavBar


viewSingle : List (Html.Attribute msg) -> List String -> List (Html msg) -> Html msg
viewSingle cellAttr headers record =
    div []
        [ table
            [ class "uk-table uk-table-responsive uk-table-divider" ]
            [ thead []
                [ viewRow th [] <| List.map text headers ]
            , tbody []
                [ viewRow td cellAttr record ]
            ]
        ]


view : List String -> List (List (Html msg)) -> Html msg
view headers rows =
    table [ class "uk-table uk-table-responsive uk-table-divider" ]
        [ thead []
            [ viewRow th [] <| List.map text headers ]
        , tbody []
            (List.map (\row -> viewRow td [] row) rows)
        ]


viewRow : (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg) -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewRow tableElement elementAttr cells =
    tr []
        (List.map
            (\content -> tableElement elementAttr [ content ])
            cells
        )


viewWithTagData : msg -> Data.Table.TableDataTagged -> Html msg
viewWithTagData exportAction { tag, headers, rows } =
    let
        plainPreparedRows =
            rows |> flattenRows |> List.map (List.map text)
    in
    div []
        [ p
            [ class "uk-position-relative" ]
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span
                    [ class "uk-text-large" ]
                    [ text tag ]
                ]
            , NavBar.viewIconNav [ ( NavBar.Export, exportAction, [] ) ]
            ]
        , view
            ("Tag" :: headers)
            plainPreparedRows
        ]
