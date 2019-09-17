module Section.ApplyTags exposing (viewBatchTaggingTab, viewManualTaggingTab)

import Data.Alias exposing (ColumnHeadingName, HtmlNodeId, SearchPattern)
import Data.Table exposing (Row)
import Dict exposing (Dict)
import Html exposing (div, p, span, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput)
import List.Extra as ListExtra
import Set exposing (Set)
import View.Autocomplete
import View.Table


viewManualTaggingTab : List ColumnHeadingName -> List String -> Html.Html msg
viewManualTaggingTab columns records =
    div []
        [ p
            [ class "uk-text-meta" ]
            [ span
                [ class "uk-label uk-text-small" ]
                [ text "NOTE" ]
            , text "  Records are processed in order provided by your file."
            ]
        , View.Table.viewSingle
            []
            columns
            (List.map text records)
        ]


viewBatchTaggingTab : Dict.Dict ColumnHeadingName SearchPattern -> (ColumnHeadingName -> SearchPattern -> msg) -> List ColumnHeadingName -> List Row -> Html.Html msg
viewBatchTaggingTab batchTaggingOptions inputAction columns records =
    div []
        [ p
            [ class "uk-text-meta" ]
            [ span
                [ class "uk-label uk-text-small" ]
                [ text "NOTE" ]
            , text "    How Batch Tagging works: Choose a column and insert a keyword to match datasets which have these keyword in a cell. Every matching dataset is then tagged by the tag you choose next."
            ]
        , viewBatchTagging batchTaggingOptions inputAction columns records
        ]


viewBatchTagging : Dict ColumnHeadingName SearchPattern -> (ColumnHeadingName -> SearchPattern -> msg) -> List ColumnHeadingName -> List Row -> Html.Html msg
viewBatchTagging batchTaggingOptions inputAction columns records =
    let
        autoTagger =
            columns
                |> List.indexedMap
                    (\index column ->
                        let
                            val =
                                Maybe.withDefault "" <| Dict.get column batchTaggingOptions

                            -- create data list for autocomplete
                            options =
                                Set.fromList <| getColumnData index records
                        in
                        viewBatchTaggingInput column ("autoTagger" ++ String.fromInt index) val options (inputAction column)
                    )
    in
    if List.isEmpty records then
        text ""

    else
        div
            []
            [ p
                [ class "uk-width-1-1" ]
                autoTagger
            ]


viewBatchTaggingInput : String -> String -> String -> Set String -> (SearchPattern -> msg) -> Html.Html msg
viewBatchTaggingInput labelText idVal val options action =
    View.Autocomplete.view
        labelText
        val
        idVal
        [ placeholder "Select a keyword (Plain or Regex)", onInput action ]
        options


getColumnData : Int -> List Row -> List String
getColumnData columnIndex records =
    List.foldl (.cells >> ListExtra.getAt columnIndex >> Maybe.withDefault "" >> List.singleton >> List.append) [] records
