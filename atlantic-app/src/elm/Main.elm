module Main exposing (main)

import Base64
import Browser
import Csv exposing (Csv)
import Data.Alias exposing (ColumnHeadingName, HtmlNodeId, SearchPattern, Tag)
import Data.Button
import Data.Modal
import Data.Structure as Structure
import Data.Table exposing (Cell, Row, TableData, TableDataTagged, flattenRows)
import Dict exposing (Dict)
import Html exposing (Html, a, button, datalist, dd, div, dl, dt, h1, h2, h3, h4, h5, hr, input, label, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, href, id, name, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import List.Extra as ListExtra
import Ports.FileReader exposing (FileData, fileContentRead, fileSelected)
import Regex exposing (Regex)
import Section.ApplyTags exposing (viewBatchTaggingTab, viewManualTaggingTab)
import Section.FileUpload
import Section.ManageTags
import Set exposing (Set)
import Unique exposing (Unique)
import View.Autocomplete as Autocomplete
import View.Modal as Modal
import View.NavBar as NavBar
import View.Table as Table
import View.Tags as Tags



{- make invalid state impossible -}


type TaggingOption
    = SingleTagging
    | BatchTagging


type UndoStrategy
    = DropLast


type alias HtmlNode =
    Html.Html Msg


type alias Model =
    { tags : Set String
    , addTagInputBuffer : String
    , addTagInputError : ( String, Bool )
    , mapTagInputBuffer : String
    , fileUploadPointerId : HtmlNodeId
    , file : Maybe FileData
    , tableData : List TableData
    , tableDataTagged : List (List TableDataTagged)
    , batchTaggingOptions : Dict ColumnHeadingName SearchPattern
    , optionTagging : TaggingOption
    , showModal : Data.Modal.State Msg
    }


type Msg
    = RemoveTag String
    | TagInput String
    | CreateTagFromBuffer
    | FileSelected
    | ParseToCsv FileData
    | MapRecordToTag (Structure.Bucket Row) (Maybe String)
    | MapTagInput String
    | SearchPatternInput ColumnHeadingName SearchPattern
    | SetTaggingOption TaggingOption
    | NoOp
    | OpenModal Data.Modal.Title HtmlNode Msg Msg
    | OpenModalInfo Data.Modal.Title HtmlNode Msg
    | CloseModal
    | UndoMapRecordToTag UndoStrategy


main : Program (List String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : List String -> ( Model, Cmd Msg )
init flags =
    let
        tags =
            [ "haushalt", "hobby", "horem", "ipsum" ]
                |> Set.fromList
    in
    ( { tags = tags
      , addTagInputBuffer = ""
      , addTagInputError = ( "", False )
      , mapTagInputBuffer = ""
      , fileUploadPointerId = "csv-upload"
      , file = Nothing
      , tableData = []
      , tableDataTagged = []
      , batchTaggingOptions = Dict.empty
      , optionTagging = SingleTagging
      , showModal = { visible = Data.Modal.NotVisible, content = text "", buttons = [], title = "" }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RemoveTag name ->
            let
                newTags =
                    Set.diff model.tags (Set.singleton name)
            in
            ( { model | tags = newTags }, Cmd.none )

        TagInput val ->
            ( { model | addTagInputBuffer = String.toLower val }, Cmd.none )

        CreateTagFromBuffer ->
            let
                newTags =
                    if String.isEmpty model.addTagInputBuffer then
                        model.tags

                    else
                        Set.insert model.addTagInputBuffer model.tags

                ( addTagInputError, addTagInputBuffer ) =
                    if Set.member model.addTagInputBuffer model.tags then
                        ( ( "Tag already exists", True ), model.addTagInputBuffer )

                    else
                        ( ( "", False ), "" )
            in
            ( { model | tags = newTags, addTagInputBuffer = addTagInputBuffer, addTagInputError = addTagInputError }, Cmd.none )

        FileSelected ->
            ( model, fileSelected model.fileUploadPointerId )

        ParseToCsv fileData ->
            case decodeFileData fileData.contents of
                Ok decodedData ->
                    let
                        tableData =
                            createTableDataFromCsvData decodedData
                    in
                    ( { model
                        | file = Just fileData
                        , tableData = [ tableData ]
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        dialogContent =
                            text ("There was an error parsing your file : " ++ error)
                    in
                    ( openModalInfo "Error" model dialogContent CloseModal, Cmd.none )

        MapRecordToTag recordBucket tag ->
            let
                theTag =
                    Maybe.withDefault model.mapTagInputBuffer tag

                tableDataOrigin =
                    Maybe.withDefault (TableData [] []) (List.head model.tableData)

                commonHeaders =
                    tableDataOrigin.headers

                tableDataTagged =
                    Maybe.withDefault [] (List.head model.tableDataTagged)

                tableDataTaggedAndPrepared =
                    setTagInstance theTag commonHeaders tableDataTagged
            in
            case recordBucket of
                Structure.Single aTableRow ->
                    let
                        updatedTaggedTableData =
                            tableDataTaggedAndPrepared
                                |> List.map (mapRowToTag theTag aTableRow)

                        updatedOriginRows =
                            Maybe.withDefault [] (List.tail tableDataOrigin.rows)

                        updatedTableDataOrigin =
                            { tableDataOrigin | rows = updatedOriginRows }
                    in
                    ( { model
                        | tableDataTagged = updatedTaggedTableData :: model.tableDataTagged
                        , tableData = updatedTableDataOrigin :: model.tableData
                      }
                    , Cmd.none
                    )

                Structure.Multiple someTableRows ->
                    let
                        updatedTableDataTagged =
                            List.map
                                (\table ->
                                    if table.tag == theTag then
                                        let
                                            updatedRows =
                                                List.concat [ table.rows, someTableRows ]
                                        in
                                        TableDataTagged theTag commonHeaders updatedRows

                                    else
                                        table
                                )
                                tableDataTaggedAndPrepared

                        updatedOriginRows =
                            tableDataOrigin.rows
                                |> List.filter
                                    (\aRow ->
                                        ListExtra.notMember
                                            aRow
                                            someTableRows
                                    )

                        updatedTableDataOrigin =
                            { tableDataOrigin | rows = updatedOriginRows }

                        updatedModel =
                            { model
                                | tableDataTagged = updatedTableDataTagged :: model.tableDataTagged
                                , tableData = updatedTableDataOrigin :: model.tableData
                            }
                    in
                    ( closeModal updatedModel, Cmd.none )

        MapTagInput val ->
            ( { model | mapTagInputBuffer = val }, Cmd.none )

        SearchPatternInput columnKey searchPatternInput ->
            case String.isEmpty searchPatternInput of
                False ->
                    ( { model | batchTaggingOptions = Dict.insert columnKey searchPatternInput model.batchTaggingOptions }, Cmd.none )

                True ->
                    ( { model | batchTaggingOptions = Dict.remove columnKey model.batchTaggingOptions }, Cmd.none )

        SetTaggingOption opt ->
            ( { model | optionTagging = opt }, Cmd.none )

        OpenModalInfo title content cancelMsg ->
            ( openModalInfo title model content cancelMsg, Cmd.none )

        OpenModal title content saveMsg cancelMsg ->
            ( openModal title model content saveMsg cancelMsg, Cmd.none )

        CloseModal ->
            ( closeModal model, Cmd.none )

        UndoMapRecordToTag undoStrategy ->
            case undoStrategy of
                DropLast ->
                    let
                        ( newHistoryData, newHistoryDataTagged ) =
                            case ( model.tableData, model.tableDataTagged ) of
                                ( firstTableD :: restTableD, firstTableDT :: restTableDT ) ->
                                    ( restTableD, restTableDT )

                                _ ->
                                    Debug.log "nothing changed" ( model.tableData, model.tableDataTagged )
                    in
                    ( { model | tableData = newHistoryData, tableDataTagged = newHistoryDataTagged }, Cmd.none )


openModalInfo :
    Data.Modal.Title
    -> Model
    -> HtmlNode
    -> Msg
    -> Model
openModalInfo title model content cancelMsg =
    let
        buttons =
            [ ( Data.Button.Secondary, cancelMsg, "Ok" )
            ]
    in
    { model | showModal = Data.Modal.State Data.Modal.Visible content buttons title }


openModal : Data.Modal.Title -> Model -> HtmlNode -> Msg -> Msg -> Model
openModal title model content saveMsg cancelMsg =
    let
        buttons =
            [ ( Data.Button.Secondary, cancelMsg, "Cancel" )
            , ( Data.Button.Secondary, saveMsg, "Save" )
            ]
    in
    { model | showModal = Data.Modal.State Data.Modal.Visible content buttons title }


closeModal : Model -> Model
closeModal model =
    { model | showModal = Data.Modal.State Data.Modal.NotVisible model.showModal.content model.showModal.buttons model.showModal.title }


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead ParseToCsv


view : Model -> HtmlNode
view model =
    let
        tableData =
            Maybe.withDefault (TableData [] []) (List.head model.tableData)

        tableDataTagged =
            Maybe.withDefault [] (List.head model.tableDataTagged)

        currentRow =
            Maybe.withDefault (Row (Unique.run Unique.unique) [] []) (List.head tableData.rows)

        taggingSectionNav =
            viewTaggingIconNav ( model.tableData, model.tableDataTagged )
    in
    div [ id "container", class "uk-container" ]
        [ Modal.view
            CloseModal
            model.showModal.visible
            model.showModal.title
            model.showModal.content
            model.showModal.buttons
        , div
            []
            [ div []
                [ Section.FileUpload.view (maybeToBool model.file) FileSelected ]
            , div []
                [ Section.ManageTags.view model.addTagInputError model.addTagInputBuffer model.tags TagInput CreateTagFromBuffer RemoveTag
                ]
            , div []
                [ viewTaggingSection model.optionTagging model.batchTaggingOptions model.tags tableData.headers currentRow tableData.rows taggingSectionNav
                ]
            ]
        , div
            [ class "row" ]
            [ div [ class "col-lg-12 col-sm-12" ]
                [ viewMappedRecordsPanel tableData.headers tableDataTagged
                ]
            ]
        ]


mapRowCellsToHaveColumns : List ColumnHeadingName -> Row -> { id : Unique.Id, cells : List ( ColumnHeadingName, Cell ), tags : List Tag }
mapRowCellsToHaveColumns headers row =
    let
        newCells =
            List.map2 Tuple.pair headers row.cells
    in
    { cells = List.map2 Tuple.pair headers row.cells, id = row.id, tags = row.tags }


mapRowCellsToRemoveColumns : { id : Unique.Id, cells : List ( ColumnHeadingName, Cell ), tags : List Tag } -> Row
mapRowCellsToRemoveColumns row =
    let
        newCells =
            List.map (\( column, cell ) -> cell) row.cells
    in
    Row row.id newCells row.tags


viewTaggingSection : TaggingOption -> Dict ColumnHeadingName SearchPattern -> Set Tag -> List ColumnHeadingName -> Row -> List Row -> HtmlNode -> HtmlNode
viewTaggingSection taggingOption batchTaggingOptions tags headers row rows nav =
    let
        {--TODO: move parts of this to the update function as a standalone action ? --}
        {--TODO: Refactor to have combined to search --}
        taggingAction tag =
            case taggingOption of
                SingleTagging ->
                    MapRecordToTag (Structure.Single row) tag

                BatchTagging ->
                    let
                        plainRows =
                            rowPlain rows

                        matchedRows =
                            rows
                                |> List.map
                                    (mapRowCellsToHaveColumns headers)
                                |> List.filter
                                    (\row_ ->
                                        let
                                            matchingCellCount =
                                                List.foldr
                                                    (\( column, cell ) count ->
                                                        case Dict.get column batchTaggingOptions of
                                                            Just searchPattern ->
                                                                let
                                                                    regexPattern =
                                                                        Regex.fromString searchPattern
                                                                            |> Maybe.withDefault Regex.never
                                                                in
                                                                if Regex.contains regexPattern cell then
                                                                    count + 1

                                                                else
                                                                    count

                                                            Nothing ->
                                                                count
                                                    )
                                                    0
                                                    row_.cells
                                        in
                                        Dict.size batchTaggingOptions == matchingCellCount
                                    )

                        matchedRowsAsRowType =
                            matchedRows
                                |> List.map mapRowCellsToRemoveColumns

                        plainMatchedRecords =
                            rowPlain matchedRowsAsRowType

                        modelTitleText =
                            String.fromInt (List.length plainMatchedRecords) ++ " Records that will be tagged"
                    in
                    if List.isEmpty plainMatchedRecords then
                        OpenModalInfo
                            modelTitleText
                            (text "There were no matching records found")
                            CloseModal

                    else
                        OpenModal
                            modelTitleText
                            (Table.view headers plainMatchedRecords)
                            (MapRecordToTag (Structure.Multiple matchedRowsAsRowType) tag)
                            CloseModal

        ( singleIsActiveTab, viewTab ) =
            case taggingOption of
                {--tab selection is naive--}
                SingleTagging ->
                    ( True, viewManualTaggingTab headers row.cells )

                BatchTagging ->
                    ( False, viewBatchTaggingTab SearchPatternInput headers rows )
    in
    if List.isEmpty row.cells then
        text ""

    else
        div []
            [ div [ class "uk-position-relative" ]
                [ h3
                    [ class "uk-heading-line uk-text-center" ]
                    [ span [ class "uk-text-background uk-text-bold uk-text-large" ]
                        [ text ("Apply tags (" ++ String.fromInt (List.length rows) ++ " left)")
                        ]
                    ]
                , nav
                ]
            , div [ class "uk-width-1-1 uk-margin-large" ]
                [ ul
                    [ class "uk-child-width-expand", attribute "uk-tab" "" ]
                    [ li
                        [ onClick (SetTaggingOption SingleTagging)
                        , classList [ ( "uk-active", singleIsActiveTab ) ]
                        ]
                        [ a [ href "#" ] [ text "Single Tagging" ] ]
                    , li
                        [ onClick (SetTaggingOption BatchTagging)
                        , classList [ ( "uk-active", not singleIsActiveTab ) ]
                        ]
                        [ a [ href "#" ] [ text "Batch Tagging" ] ]
                    ]
                ]
            , viewTab
            , hr [ class "uk-divider-icon" ] []
            , Tags.viewTagCloud (\tag -> taggingAction (Just tag)) tags
            ]


viewTaggingIconNav : ( List a, List a1 ) -> Html Msg
viewTaggingIconNav ( history1, history2 ) =
    {--naive, consider tuple in model & origin data as own field in model--}
    let
        ( history1Length, history2Length ) =
            ( List.length history1, List.length history2 )

        validHistory =
            (toFloat (history1Length - 1) / toFloat history2Length) == 1

        undoButton =
            if validHistory && history1Length > 1 then
                ( NavBar.Undo, UndoMapRecordToTag DropLast, [] )

            else
                ( NavBar.Disabled NavBar.Undo, NoOp, [] )
    in
    NavBar.viewIconNav
        [ undoButton
        , ( NavBar.Disabled NavBar.Redo, NoOp, [] )
        , ( NavBar.Disabled NavBar.Backward, NoOp, [] )
        , ( NavBar.Disabled NavBar.Forward, NoOp, [] )
        ]


viewMappedRecordsPanel : List String -> List TableDataTagged -> Html Msg
viewMappedRecordsPanel headers someTables =
    if List.isEmpty someTables then
        text ""

    else
        let
            rows =
                someTables
                    |> List.map Table.viewWithTagData
                    |> List.map (\viewWithTagData -> viewWithTagData NoOp)
        in
        div []
            ([ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span
                    [ class "uk-text-background uk-text-bold uk-text-large" ]
                    [ text "Tagged records" ]
                ]
             ]
                ++ rows
            )


viewPlainRecords : String -> List String -> List Row -> Html msg
viewPlainRecords descr headers rows =
    if List.isEmpty rows then
        text ""

    else
        div []
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span [] [ text descr ]
                ]
            , Table.view headers (flattenRows rows)
            ]


setTagInstance : Tag -> List ColumnHeadingName -> List TableDataTagged -> List TableDataTagged
setTagInstance theTag headers tableDataTagged =
    let
        doesInstanceExist =
            List.any (\model -> model.tag == theTag) tableDataTagged
    in
    if not doesInstanceExist then
        { headers = headers, tag = theTag, rows = [] } :: tableDataTagged

    else
        tableDataTagged


getColIndex : List String -> String -> Maybe Int
getColIndex columns colName =
    ListExtra.elemIndex colName columns


mapRowToTag : String -> Row -> TableDataTagged -> TableDataTagged
mapRowToTag aTag aRow { headers, tag, rows } =
    if tag == aTag then
        { headers = headers, tag = tag, rows = aRow :: rows }

    else
        { headers = headers, tag = tag, rows = rows }


rowPlain : List Row -> List (List String)
rowPlain recordList =
    List.map (\record -> record.cells) recordList


decodeFileData : String -> Result String String
decodeFileData encodedData =
    encodedData
        |> Base64.decode


createTableDataFromCsvData : String -> TableData
createTableDataFromCsvData contents =
    let
        csv =
            contents
                |> Csv.parseWith ";"

        uniqueIds =
            Unique.replicate (List.length csv.records) Unique.unique
                |> Unique.run

        recordsConvertedToRows =
            List.map2 Row uniqueIds csv.records
                |> List.map (\rowUnderConstruction -> rowUnderConstruction [])
    in
    TableData csv.headers recordsConvertedToRows


maybeToBool : Maybe a -> Bool
maybeToBool aMaybe =
    case aMaybe of
        Just something ->
            True

        Nothing ->
            False
