module Section.ApplyTags exposing (viewBatchTaggingTab, viewManualTaggingTab)

import Data.Alias exposing (ColumnHeadingName, HtmlNodeId, SearchPattern)
import Data.Locale as Locale
import Data.Table exposing (Row)
import Dict exposing (Dict)
import Html exposing (div, p, span, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput)
import Html.Lazy
import List.Extra as ListExtra
import Set exposing (Set)
import View.Autocomplete
import View.Table


viewManualTaggingTab : Locale.Locale -> List ColumnHeadingName -> List String -> Html.Html msg
viewManualTaggingTab locale columns records =
    let
        content =
            if List.isEmpty records then
                [ text <| Locale.translateNoRecordsToChooseFromSelectAfile locale ]

            else
                [ p
                    [ class "uk-text-meta" ]
                    [ span
                        [ class "uk-label uk-text-small" ]
                        [ text "NOTE" ]
                    , text <| "  " ++ Locale.translateHowManualTaggingWorks locale
                    ]
                , View.Table.viewSingle
                    []
                    columns
                    (List.map text records)
                ]
    in
    div [] content


viewBatchTaggingTab : Locale.Locale -> Dict.Dict ColumnHeadingName SearchPattern -> (ColumnHeadingName -> SearchPattern -> msg) -> List ColumnHeadingName -> List Row -> Html.Html msg
viewBatchTaggingTab locale batchTaggingOptions inputAction columns records =
    let
        content =
            if List.isEmpty records then
                [ text <| Locale.translateNoRecordsToChooseFromSelectAfile locale ]

            else
                [ p
                    [ class "uk-text-meta" ]
                    [ span
                        [ class "uk-label uk-text-small" ]
                        [ text "NOTE" ]
                    , span [ class "uk-text-small uk-text-light" ] [ text <| "   " ++ Locale.translateHowBatchTaggingWorks locale ]
                    ]
                , viewBatchTagging locale batchTaggingOptions inputAction columns records
                ]
    in
    div [] content


viewBatchTagging : Locale.Locale -> Dict ColumnHeadingName SearchPattern -> (ColumnHeadingName -> SearchPattern -> msg) -> List ColumnHeadingName -> List Row -> Html.Html msg
viewBatchTagging locale batchTaggingOptions inputAction columns records =
    let
        autoTagger =
            columns
                |> List.indexedMap
                    (Html.Lazy.lazy2
                        (\index column ->
                            let
                                val =
                                    Maybe.withDefault "" <| Dict.get column batchTaggingOptions

                                -- create data list for autocomplete
                                options =
                                    Set.fromList <| Data.Table.getColumnData index records
                            in
                            Html.Lazy.lazy6 viewBatchTaggingInput column locale ("autoTagger" ++ String.fromInt index) val options (inputAction column)
                        )
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


viewBatchTaggingInput : String -> Locale.Locale -> String -> String -> Set String -> (SearchPattern -> msg) -> Html.Html msg
viewBatchTaggingInput labelText locale idVal val options action =
    View.Autocomplete.view
        labelText
        val
        idVal
        [ placeholder (Locale.translateSelectAKeywordOrRegex locale), onInput action ]
        options
