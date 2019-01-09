module View.Table exposing (view, viewSingle, viewWithTagData)

import Data.Table exposing (Row, flattenRows, prependCellToRow)
import Html exposing (Html, div, h3, h4, h5, p, small, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)


viewSingle : List (Html.Attribute msg) -> List String -> List String -> Html msg
viewSingle cellAttr headers record =
    div []
        [ table
            [ class "uk-table uk-table-responsive uk-table-divider" ]
            [ thead []
                [ viewRow (th) [] headers ]
            , tbody []
                [ viewRow (td) cellAttr record ]
            ]
        ]


view : List String -> List (List String) -> Html msg
view headers rows =
    table [ class "uk-table uk-table-responsive uk-table-divider" ]
        [ thead []
            [ viewRow (th) [] headers ]
        , tbody []
            (List.map (\row -> viewRow (td) [] row) rows)
        ]


viewRow : (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg) -> List (Html.Attribute msg) -> List String -> Html msg
viewRow tableElement elementAttr cells =
    tr []
        (List.map
            (\content -> tableElement elementAttr [ text content ])
            cells
        )


viewWithTagData : Data.Table.TableDataTagged -> Html msg
viewWithTagData { tag, headers, rows } =
    let
        plainPreparedRows =
            List.map (prependCellToRow tag) rows |> flattenRows
    in
        div []
            [ p
                []
                [ h4 [] [ text tag ] ]
            , view
                ("Tag" :: headers)
                plainPreparedRows
            ]
