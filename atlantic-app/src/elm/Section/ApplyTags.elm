module Section.ApplyTags exposing (viewAutoTaggingTab, viewManualTaggingTab)

import Data.Alias exposing (ColumnHeadingName, HtmlNodeId)
import Data.Table exposing (Row)
import Html exposing (div, p, span, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput)
import List.Extra as ListExtra
import Set exposing (Set)
import View.Autocomplete
import View.Input exposing (viewRadioGroup)
import View.Table


viewManualTaggingTab : List ColumnHeadingName -> List String -> Html.Html msg
viewManualTaggingTab headers columns =
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
            headers
            columns
        ]


viewAutoTaggingTab : Int -> (String -> msg) -> (String -> msg) -> List ColumnHeadingName -> List Row -> Html.Html msg
viewAutoTaggingTab colIndex inputAction setPointerAction headers records =
    div []
        [ p
            [ class "uk-text-meta" ]
            [ span
                [ class "uk-label uk-text-small" ]
                [ text "NOTE" ]
            , text "    How Batch Tagging works: Choose a column and insert a keyword to match datasets which have these keyword in a cell. Every matching dataset is then tagged by the tag you choose next."
            ]
        , viewAutoRecordMapper colIndex inputAction setPointerAction headers records
        ]


viewAutoRecordMapper : Int -> (String -> msg) -> (String -> msg) -> List String -> List Row -> Html.Html msg
viewAutoRecordMapper colIndex inputAction setPointerAction headers records =
    let
        {- how does this make sense ? -}
        autoTagOptions =
            records
                |> List.map (\record -> ListExtra.getAt colIndex record.cells)
                |> List.map (\a -> Maybe.withDefault "" a)
                |> ListExtra.dropWhile (\str -> String.isEmpty str)

        autoTagger =
            if List.isEmpty autoTagOptions then
                text ""

            else
                viewAutoTagger (Set.fromList autoTagOptions) inputAction
    in
    if List.isEmpty records then
        text ""

    else
        div
            []
            [ p
                [ class "uk-width-1-1" ]
                (viewRadioGroup "autoTagPointer" setPointerAction headers
                    ++ [ autoTagger ]
                )
            ]


viewAutoTagger : Set String -> (String -> msg) -> Html.Html msg
viewAutoTagger options action =
    View.Autocomplete.view
        "autoTagger"
        [ placeholder "Select a keyword (Plain or Regex)", onInput action ]
        options
