port module Main exposing (main)

import Browser
import Csv
import Data.Alias exposing (ColumnHeadingName, HtmlNodeId, SearchPattern, Tag)
import Data.Button
import Data.Modal
import Data.Table exposing (Cell, Row, TableData, TableDataTagged, decodeTableDataList, decodeTableDataTaggedList, encodeRow, encodeTableData, encodeTableDataTagged, flattenRows, prependCellToRow)
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html, a, button, datalist, dd, div, dl, dt, h1, h2, h3, h4, h5, hr, input, label, li, option, p, select, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, href, id, name, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListExtra
import Ports.FileReader exposing (FileData, decodeFileContents, decodeFileData, encodeFileData, fileContentRead, fileSelected)
import Regex
import Section.ApplyTags exposing (viewBatchTaggingTab, viewManualTaggingTab)
import Section.FileUpload
import Section.ManageTags
import Set exposing (Set)
import View.Modal as Modal
import View.NavBar as NavBar
import View.Table as Table
import View.Tags as Tags



{- make invalid state impossible -}
-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , subscriptions = subscriptions
        , view = view
        }



-- PORT


port setStorage : Encode.Value -> Cmd msg



-- TYPES


type TaggingOption
    = SingleTagging
    | BatchTagging


type UndoStrategy
    = DropLast


type alias HtmlNode =
    Html.Html Msg


type alias EncodedDataFormat =
    String


type DataFormat
    = Text
    | Float
    | Integer
    | Date
    | Currency Currency


type Currency
    = Dollar
    | Euro


type DateFormat
    = DDMMYYYY Separator
    | YYYYMMDD Separator


type Separator
    = Slash
    | Dot
    | Space
    | Dash


{-| data of type Bucket can be either a single instance of type `a`,
or a List of type `a`
-}
type Bucket a
    = Single a
    | Multiple (List a)



-- MODEL


type alias Model =
    { tags : Set Tag
    , addTagInputBuffer : String
    , addTagInputError : ( String, Bool )
    , fileUploadPointerId : HtmlNodeId
    , file : Maybe FileData
    , tableData : List TableData
    , tableDataTagged : List (List TableDataTagged)
    , batchTaggingOptions : Dict ColumnHeadingName SearchPattern
    , dataFormats : Dict ColumnHeadingName DataFormat
    , optionTagging : TaggingOption
    , showModal : Data.Modal.State Msg
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        tags =
            case Decode.decodeValue (Decode.field "tags" (Decode.list Decode.string)) flags of
                Ok val ->
                    Set.fromList val

                Err _ ->
                    Set.fromList [ "finance", "household", "expenses" ]

        addTagInputBuffer =
            Result.withDefault "" <| Decode.decodeValue (Decode.field "addTagInputBuffer" Decode.string) flags

        addTagInputError =
            Result.withDefault ( "", False ) <|
                Decode.decodeValue
                    (Decode.field "addTagInputError" (Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.bool)))
                    flags

        fileUploadPointerId =
            Result.withDefault "csv-upload" <| Decode.decodeValue (Decode.field "fileUploadPointerId" Decode.string) flags

        file =
            decodeFileData flags "file"

        tableData =
            Result.withDefault [] <| decodeTableDataList flags "tableData"

        tableDataTagged =
            Result.withDefault [] <| decodeTableDataTaggedList flags "tableDataTagged"

        batchTaggingOptions =
            Result.withDefault Dict.empty <| Decode.decodeValue (Decode.field "batchTaggingOptions" (Decode.dict Decode.string)) flags

        dataFormats =
            Result.withDefault Dict.empty <| Decode.decodeValue (Decode.field "dataFormats" (Decode.dict dataFormat)) flags
    in
    ( { tags = tags
      , addTagInputBuffer = addTagInputBuffer
      , addTagInputError = addTagInputError
      , fileUploadPointerId = fileUploadPointerId
      , file = file
      , tableData = tableData
      , tableDataTagged = tableDataTagged
      , batchTaggingOptions = batchTaggingOptions
      , dataFormats = dataFormats
      , optionTagging = BatchTagging
      , showModal = { visible = Data.Modal.NotVisible, content = text "", buttons = [], title = "", displayProperties = Data.Modal.Fullscreen }
      }
    , Cmd.none
    )


tuple2Encoder : (a -> Encode.Value) -> (b -> Encode.Value) -> ( a, b ) -> Encode.Value
tuple2Encoder enc1 enc2 ( val1, val2 ) =
    Encode.list identity [ enc1 val1, enc2 val2 ]


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "tags", Encode.set Encode.string model.tags )
        , ( "addTagInputBuffer", Encode.string model.addTagInputBuffer )
        , ( "addTagInputError", tuple2Encoder Encode.string Encode.bool model.addTagInputError )
        , ( "fileUploadPointerId", Encode.string model.fileUploadPointerId )
        , ( "file", encodeFileData model.file )
        , ( "tableData", Encode.list encodeTableData model.tableData )
        , ( "tableDataTagged", Encode.list (Encode.list encodeTableDataTagged) model.tableDataTagged )
        , ( "batchTaggingOptions", Encode.dict identity Encode.string model.batchTaggingOptions )
        , ( "dataFormats", Encode.dict identity encodeDataFormat model.dataFormats )
        ]


encodeDataFormat : DataFormat -> Encode.Value
encodeDataFormat dataFormat_ =
    case dataFormat_ of
        Text ->
            Encode.string "text"

        Float ->
            Encode.string "float"

        Integer ->
            Encode.string "integer"

        Date ->
            Encode.string "date"

        Currency Euro ->
            Encode.string "currency-euro"

        Currency Dollar ->
            Encode.string "currency-dollar"


dataFormat : Decode.Decoder DataFormat
dataFormat =
    Decode.string |> Decode.andThen dataFormatDecoder


dataFormatDecoder : String -> Decode.Decoder DataFormat
dataFormatDecoder encodedFormat =
    case parseDataFormat encodedFormat of
        Ok dataFormat_ ->
            Decode.succeed dataFormat_

        Err err ->
            Decode.fail err


parseDataFormat : String -> Result String DataFormat
parseDataFormat encodedFormat =
    case encodedFormat of
        "text" ->
            Ok Text

        "float" ->
            Ok Float

        "integer" ->
            Ok Integer

        "date" ->
            Ok Date

        "currency-euro" ->
            Ok (Currency Euro)

        "currency-dollar" ->
            Ok (Currency Dollar)

        _ ->
            Err "Invalid DataFormat Encoding"



-- UPDATE


type Msg
    = RemoveTag String
    | TagInput String
    | CreateTagFromBuffer
    | FileSelected
    | ParseToCsv FileData
    | MapRecordToTag (Bucket Row) Tag
    | SearchPatternInput ColumnHeadingName SearchPattern
    | SetTaggingOption TaggingOption
    | NoOp
    | OpenModal Data.Modal.Title HtmlNode Msg Msg
    | OpenModalInfo Data.Modal.Title HtmlNode Msg
    | CloseModal
    | UndoMapRecordToTag UndoStrategy
    | TableDownload TableDataTagged
    | SetDataFormatForColumn ColumnHeadingName EncodedDataFormat
    | SelectMatchingRecords (List ColumnHeadingName) Tag (List Row)
    | SortTaggedTable Tag ColumnHeadingName
    | ChooseDataFormat


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel, Cmd.batch [ setStorage (encodeModel newModel), cmds ] )


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
            case decodeFileContents fileData.contents of
                Ok decodedData ->
                    let
                        newModel =
                            case parseCsvString ';' decodedData of
                                Ok csv ->
                                    createTableDataFromCsv csv model |> updateShowModalChooseDataFormat csv.headers

                                Err _ ->
                                    case parseCsvString ',' decodedData of
                                        Ok csv ->
                                            createTableDataFromCsv csv model |> updateShowModalChooseDataFormat csv.headers

                                        Err err ->
                                            updateShowModalInfo "Error" model (text "There was an error parsing your file. The contents of your file are not supported.") CloseModal
                    in
                    ( newModel
                    , Cmd.none
                    )

                Err error ->
                    let
                        dialogContent =
                            text ("There was an error reading your file : " ++ error)
                    in
                    ( updateShowModalInfo "Error" model dialogContent CloseModal, Cmd.none )

        MapRecordToTag recordBucket theTag ->
            let
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
                Single aTableRow ->
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

                Multiple someTableRows ->
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
                    ( updateCloseModal updatedModel, Cmd.none )

        SearchPatternInput columnKey searchPatternInput ->
            if String.isEmpty searchPatternInput then
                ( { model | batchTaggingOptions = Dict.remove columnKey model.batchTaggingOptions }, Cmd.none )

            else
                ( { model | batchTaggingOptions = Dict.insert columnKey searchPatternInput model.batchTaggingOptions }, Cmd.none )

        SetTaggingOption opt ->
            ( { model | optionTagging = opt }, Cmd.none )

        OpenModalInfo title content cancelMsg ->
            ( updateShowModalInfo title model content cancelMsg, Cmd.none )

        OpenModal title content saveMsg cancelMsg ->
            ( updateShowModal Data.Modal.Fullscreen title model content saveMsg cancelMsg, Cmd.none )

        CloseModal ->
            ( updateCloseModal model, Cmd.none )

        UndoMapRecordToTag undoStrategy ->
            case undoStrategy of
                DropLast ->
                    let
                        ( newHistoryData, newHistoryDataTagged ) =
                            case ( model.tableData, model.tableDataTagged ) of
                                ( firstTableD :: restTableD, firstTableDT :: restTableDT ) ->
                                    ( restTableD, restTableDT )

                                _ ->
                                    ( model.tableData, model.tableDataTagged )

                        {- Debug.log "nothing changed" ( model.tableData, model.tableDataTagged ) -}
                    in
                    ( { model | tableData = newHistoryData, tableDataTagged = newHistoryDataTagged }, Cmd.none )

        TableDownload { tag, headers, rows } ->
            {- expected that each row has the tag name prepended -}
            let
                preparedHeaders =
                    setCSVSemicolonsInList ("Tag" :: headers)

                preparedRows =
                    List.map setCSVSemicolonsInList <| flattenRows rows

                theTable =
                    preparedHeaders
                        :: preparedRows

                theCsvString =
                    List.foldr String.append "" <| List.map (\row -> List.foldr String.append "" row) theTable
            in
            ( model, Download.string (tag ++ "-table.csv") "text/csv" theCsvString )

        SetDataFormatForColumn column encodedFormat ->
            let
                modelWithDataFormat =
                    updateDataFormat column encodedFormat model
            in
            ( modelWithDataFormat, Cmd.none )

        SelectMatchingRecords headers tag rows ->
            let
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
                                                case Dict.get column model.batchTaggingOptions of
                                                    Just searchPattern ->
                                                        let
                                                            regexPattern =
                                                                Regex.fromStringWith { caseInsensitive = True, multiline = False } searchPattern
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
                                Dict.size model.batchTaggingOptions == matchingCellCount
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
                ( updateShowModalInfo modelTitleText model (text "There were no matching records found") CloseModal, Cmd.none )

            else
                ( updateShowModal
                    Data.Modal.Fullscreen
                    modelTitleText
                    model
                    (Table.view (List.map (\column -> ( column, NoOp )) headers) <| List.map (List.map text) plainMatchedRecords)
                    (MapRecordToTag (Multiple matchedRowsAsRowType) tag)
                    CloseModal
                , Cmd.none
                )

        SortTaggedTable theTagToLookup column ->
            let
                currentTableDataTaggedList =
                    List.head model.tableDataTagged |> Maybe.withDefault []

                currentTableDataTaggedByTag =
                    ListExtra.find (.tag >> (==) theTagToLookup) currentTableDataTaggedList

                indexCurrentTableDataTaggedByTag =
                    ListExtra.findIndex (.tag >> (==) theTagToLookup) currentTableDataTaggedList
            in
            case ( currentTableDataTaggedByTag, indexCurrentTableDataTaggedByTag ) of
                ( Just { tag, headers, rows }, Just theIndexCurrentTableDataTaggedByTag ) ->
                    case ListExtra.elemIndex column headers of
                        Just colummnIndex ->
                            let
                                sortedRows =
                                    sort2dListByColumn colummnIndex (rowPlain rows)
                                        |> List.map Row

                                newTableDataTagged =
                                    TableDataTagged tag headers sortedRows

                                newTableDataTaggedList =
                                    ListExtra.setAt theIndexCurrentTableDataTaggedByTag newTableDataTagged currentTableDataTaggedList
                            in
                            ( { model | tableDataTagged = List.append [ newTableDataTaggedList ] model.tableDataTagged }, Cmd.none )

                        Nothing ->
                            ( updateShowModalInfo "Sorting Tables" model (text "Index for TableData.Header lookup failed.") CloseModal, Cmd.none )

                _ ->
                    ( updateShowModalInfo "Sorting Tables" model (text "TableData lookup failed.") CloseModal, Cmd.none )

        ChooseDataFormat ->
            case List.head model.tableData of
                Just { headers } ->
                    ( updateShowModalChooseDataFormat headers model, Cmd.none )

                Nothing ->
                    ( updateShowModalInfo "Error" model (text "Please select a file to work with first. Your file may be empty.") CloseModal, Cmd.none )


updateDataFormat : ColumnHeadingName -> String -> Model -> Model
updateDataFormat column encodedFormat model =
    case parseDataFormat encodedFormat of
        Ok format ->
            { model | dataFormats = Dict.insert column format model.dataFormats }

        Err err ->
            updateShowModalInfo "Error" model (text err) CloseModal


updateShowModalChooseDataFormat : List String -> Model -> Model
updateShowModalChooseDataFormat headers model =
    let
        selectDataformat =
            headers
                |> List.map
                    (\column ->
                        select [ onInput (SetDataFormatForColumn column), class "uk-select" ]
                            [ option [ value "text" ] [ text "Text" ]
                            , option [ value "float" ] [ text "Float" ]
                            , option [ value "integer" ] [ text "Integer" ]
                            , option [ value "date" ] [ text "Date" ]
                            , option [ value "currency-euro" ] [ text "Currency Euro" ]
                            , option [ value "currency-dollar" ] [ text "Currency Dollar" ]
                            ]
                    )

        dataFormatModalContent =
            div []
                [ h4 [ class "uk-text-emphasis" ] [ text "Can you tell me the dataformats of each column of your table ?" ]
                , Table.viewSingle [] headers selectDataformat
                ]
    in
    updateShowModalInfo "Choose Dataformats" model dataFormatModalContent CloseModal


updateShowModalInfo : Data.Modal.Title -> Model -> HtmlNode -> Msg -> Model
updateShowModalInfo title model content cancelMsg =
    let
        buttons =
            [ ( Data.Button.Secondary, cancelMsg, "Ok" )
            ]
    in
    { model | showModal = Data.Modal.State Data.Modal.Visible content buttons title Data.Modal.RegularView }


updateShowModal : Data.Modal.DisplayProperties -> Data.Modal.Title -> Model -> HtmlNode -> Msg -> Msg -> Model
updateShowModal displayProperties title model content saveMsg cancelMsg =
    let
        buttons =
            [ ( Data.Button.Secondary, cancelMsg, "Cancel" )
            , ( Data.Button.Secondary, saveMsg, "Save" )
            ]
    in
    { model | showModal = Data.Modal.State Data.Modal.Visible content buttons title displayProperties }


updateCloseModal : Model -> Model
updateCloseModal model =
    { model | showModal = Data.Modal.State Data.Modal.NotVisible model.showModal.content model.showModal.buttons model.showModal.title model.showModal.displayProperties }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead ParseToCsv



-- VIEW


view : Model -> HtmlNode
view model =
    let
        tableData =
            Maybe.withDefault (TableData [] []) (List.head model.tableData)

        tableDataTagged =
            Maybe.withDefault [] (List.head model.tableDataTagged)

        currentRow =
            Maybe.withDefault (Row []) (List.head tableData.rows)

        taggingSectionNav =
            viewTaggingIconNav ( model.tableData, model.tableDataTagged )
    in
    div [ id "container", class "uk-container" ]
        [ Modal.view
            model.showModal.displayProperties
            CloseModal
            model.showModal.visible
            model.showModal.title
            model.showModal.content
            model.showModal.buttons
        , div
            []
            [ div []
                [ button [ class "uk-button uk-button-text", onClick ChooseDataFormat ] [ text "I want to reset my dataformats" ] ]
            , div []
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


viewTaggingSection : TaggingOption -> Dict ColumnHeadingName SearchPattern -> Set Tag -> List ColumnHeadingName -> Row -> List Row -> HtmlNode -> HtmlNode
viewTaggingSection taggingOption batchTaggingOptions tags headers row rows nav =
    let
        taggingAction tag =
            case taggingOption of
                SingleTagging ->
                    MapRecordToTag (Single row) tag

                BatchTagging ->
                    SelectMatchingRecords headers tag rows

        ( singleIsActiveTab, viewTab ) =
            case taggingOption of
                {--tab selection is naive--}
                SingleTagging ->
                    ( True, viewManualTaggingTab headers row.cells )

                BatchTagging ->
                    ( False, viewBatchTaggingTab batchTaggingOptions SearchPatternInput headers rows )
    in
    div []
        [ div [ class "uk-position-relative" ]
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span [ class "uk-text-background uk-text-large" ]
                    [ text ("Apply tags (" ++ String.fromInt (List.length rows) ++ " left)")
                    ]
                ]
            , nav
            ]
        , div [ class "uk-padding" ]
            [ div [ class "uk-width-1-1 uk-margin-large" ]
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
            , div [ class "uk-margin" ] [ h5 [ class "uk-text-primary" ] [ text "Select a tag to tag your records:" ] ]
            , Tags.viewTagCloud (\tag -> taggingAction tag) tags
            , hr [ class "uk-divider-icon" ] []
            ]
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
viewMappedRecordsPanel headers_ someTables =
    if List.isEmpty someTables then
        text ""

    else
        let
            preparedRows : List { tag : Tag, headers : List ( ColumnHeadingName, Msg ), rows : List Row }
            preparedRows =
                List.map
                    (\{ tag, headers, rows } ->
                        let
                            headersWithSortMsg =
                                List.map (\column -> ( column, SortTaggedTable tag column )) headers
                                    |> List.append [ ( "Tag", NoOp ) ]
                        in
                        { tag = tag, headers = headersWithSortMsg, rows = List.map (prependCellToRow tag) rows }
                    )
                    someTables

            rowsViews =
                preparedRows
                    |> List.map
                        (\tableDataTagged ->
                            Table.viewWithTagData
                                -- use original header list for Tabledownload, since we modified it before
                                (TableDownload <| TableDataTagged tableDataTagged.tag headers_ tableDataTagged.rows)
                                tableDataTagged
                        )
        in
        div []
            ([ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span
                    [ class "uk-text-background uk-text-large" ]
                    [ text "Tagged records" ]
                ]
             ]
                ++ rowsViews
            )


viewPlainRecords : String -> List String -> List Row -> Html Msg
viewPlainRecords descr headers rows =
    if List.isEmpty rows then
        text ""

    else
        div []
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span [] [ text descr ]
                ]
            , Table.view (List.map (\column -> ( column, NoOp )) headers) <| List.map (List.map text) <| flattenRows rows
            ]



-- HELPERS


getColumnData : Int -> List Row -> List String
getColumnData columnIndex records =
    List.foldl (\row newList -> [ Maybe.withDefault "" <| ListExtra.getAt columnIndex row.cells ] ++ newList) [] records


mapRowCellsToHaveColumns : List ColumnHeadingName -> Row -> { cells : List ( ColumnHeadingName, Cell ) }
mapRowCellsToHaveColumns headers row =
    { cells = List.map2 Tuple.pair headers row.cells }


mapRowCellsToRemoveColumns : { cells : List ( ColumnHeadingName, Cell ) } -> Row
mapRowCellsToRemoveColumns row =
    let
        newCells =
            List.map (\( column, cell ) -> cell) row.cells
    in
    Row newCells


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
    List.map .cells recordList


parseCsvString separator contents =
    contents
        |> Csv.parseWith separator


createTableDataFromCsv : Csv.Csv -> Model -> Model
createTableDataFromCsv csv model =
    let
        recordsConvertedToRows =
            List.map Row <| csv.records
    in
    { model | tableData = [ TableData csv.headers recordsConvertedToRows ] }


maybeToBool : Maybe a -> Bool
maybeToBool aMaybe =
    case aMaybe of
        Just something ->
            True

        Nothing ->
            False


setCSVSemicolonsInList : List String -> List String
setCSVSemicolonsInList aList =
    let
        listLengh =
            List.length aList
    in
    List.indexedMap
        (\index val ->
            let
                cleanedVal =
                    if val == "\"" then
                        ""

                    else
                        val
            in
            if index /= 0 then
                if index == listLengh - 1 then
                    ";" ++ cleanedVal ++ "\n"

                else
                    ";" ++ cleanedVal

            else
                cleanedVal
        )
        aList


sort2dListByColumn : Int -> List (List comparable) -> List (List comparable)
sort2dListByColumn index the2dList =
    List.sortWith (compareTwoListsByIndex index) the2dList


compareTwoListsByIndex : Int -> List comparable -> List comparable -> Order
compareTwoListsByIndex index firstList lastList =
    case ( ListExtra.getAt index firstList, ListExtra.getAt index lastList ) of
        ( Just firstItem, Just nextItem ) ->
            compare firstItem nextItem

        ( Nothing, Just nextItem ) ->
            GT

        ( Just firstItem, Nothing ) ->
            LT

        ( Nothing, Nothing ) ->
            LT
