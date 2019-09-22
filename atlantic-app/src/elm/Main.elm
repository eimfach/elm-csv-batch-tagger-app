port module Main exposing (main)

import Browser
import Csv
import Data.Alias exposing (ColumnHeadingName, HtmlNodeId, SearchPattern, Tag)
import Data.Button
import Data.Helpers exposing (maybeToBool)
import Data.Locale exposing (..)
import Data.Modal
import Data.Table exposing (Cell, Row, TableData, TableDataTagged, decodeTableDataList, decodeTableDataTaggedList, encodeTableData, encodeTableDataTagged, flattenRows, getColumnData, getColumnDataWith, getColumnDataWithParser, prependCellToRow)
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html, a, button, datalist, dd, div, dl, dt, h1, h2, h3, h4, h5, hr, input, label, li, option, p, select, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, href, id, name, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListExtra
import Parser exposing ((|.), (|=))
import Ports.FileReader exposing (FileData, decodeFileContents, decodeFileData, encodeFileData, fileContentRead, fileSelected)
import Regex
import Section.ApplyTags exposing (viewBatchTaggingTab, viewManualTaggingTab)
import Section.FileUpload
import Section.ManageTags
import Set exposing (Set)
import Time
import Time.Extra
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


port getLocale : (String -> msg) -> Sub msg



-- TYPES


type alias HtmlNode =
    Html.Html Msg


type alias Comparison =
    String -> String -> Order


type TaggingOption
    = SingleTagging
    | BatchTagging


type UndoStrategy
    = DropLast


type ModalContent
    = ViewMapRecordsToTag (List ColumnHeadingName) (List (List String)) Tag
    | ViewInfo String


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
    { locale : Locale
    , tags : Set Tag
    , addTagInputBuffer : String
    , addTagInputError : ( String, Bool )
    , fileUploadPointerId : HtmlNodeId
    , file : Maybe FileData
    , tableData : List TableData
    , tableDataTagged : List (List TableDataTagged)
    , batchTaggingOptions : Dict ColumnHeadingName SearchPattern
    , dataFormats : Dict ColumnHeadingName DataFormat
    , optionTagging : TaggingOption
    , showModal : Maybe (Data.Modal.State ModalContent)
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        locale =
            Result.withDefault getDefaultLocale <| Decode.decodeValue (Decode.field "locale" localeDecoder) flags

        tags =
            case Decode.decodeValue (Decode.field "tags" (Decode.list Decode.string)) flags of
                Ok val ->
                    Set.fromList val

                Err _ ->
                    Set.fromList (translateDefaultTags locale)

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
            Result.withDefault Dict.empty <| Decode.decodeValue (Decode.field "dataFormats" (Decode.dict dataFormatDecoder)) flags

        showModal =
            Result.withDefault Nothing <| Decode.decodeValue (Decode.field "showModal" (Decode.nullable (Data.Modal.createStateDecoder modalContentDecoder))) flags
    in
    ( { locale = locale
      , tags = tags
      , addTagInputBuffer = addTagInputBuffer
      , addTagInputError = addTagInputError
      , fileUploadPointerId = fileUploadPointerId
      , file = file
      , tableData = tableData
      , tableDataTagged = tableDataTagged
      , batchTaggingOptions = batchTaggingOptions
      , dataFormats = dataFormats
      , optionTagging = BatchTagging
      , showModal = showModal
      }
    , Cmd.none
    )


getOriginalTableData : Model -> Maybe TableData
getOriginalTableData model =
    List.head (List.reverse model.tableData)


tuple2Encoder : (a -> Encode.Value) -> (b -> Encode.Value) -> ( a, b ) -> Encode.Value
tuple2Encoder enc1 enc2 ( val1, val2 ) =
    Encode.list identity [ enc1 val1, enc2 val2 ]


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "locale", encodeLocale model.locale )
        , ( "tags", Encode.set Encode.string model.tags )
        , ( "addTagInputBuffer", Encode.string model.addTagInputBuffer )
        , ( "addTagInputError", tuple2Encoder Encode.string Encode.bool model.addTagInputError )
        , ( "fileUploadPointerId", Encode.string model.fileUploadPointerId )
        , ( "file", encodeFileData model.file )
        , ( "tableData", Encode.list encodeTableData model.tableData )
        , ( "tableDataTagged", Encode.list (Encode.list encodeTableDataTagged) model.tableDataTagged )
        , ( "batchTaggingOptions", Encode.dict identity Encode.string model.batchTaggingOptions )
        , ( "dataFormats", Encode.dict identity encodeDataFormat model.dataFormats )
        , ( "showModal", encodeShowModal model.showModal )
        ]


encodeShowModal : Maybe (Data.Modal.State ModalContent) -> Encode.Value
encodeShowModal showModal =
    case showModal of
        Just modalState ->
            Data.Modal.encodeState modalState encodeModalContent

        Nothing ->
            Encode.null


modalContentDecoder : Decode.Decoder ModalContent
modalContentDecoder =
    Decode.oneOf
        [ Decode.field "viewMapRecordsToTag"
            (Decode.map3 ViewMapRecordsToTag
                (Decode.field "columns" <| Decode.list Decode.string)
                (Decode.field "records" <| Decode.list (Decode.list Decode.string))
                (Decode.field "tag" Decode.string)
            )
        , Decode.field "viewInfo" (Decode.string |> Decode.map ViewInfo)
        ]


encodeModalContent : ModalContent -> Encode.Value
encodeModalContent modalContent_ =
    case modalContent_ of
        ViewMapRecordsToTag columns records tag ->
            let
                valuesEncoding =
                    Encode.object
                        [ ( "columns", Encode.list Encode.string columns )
                        , ( "records", Encode.list (Encode.list Encode.string) records )
                        , ( "tag", Encode.string tag )
                        ]
            in
            Encode.object
                [ ( "viewMapRecordsToTag", valuesEncoding )
                ]

        ViewInfo someText ->
            Encode.object
                [ ( "viewInfo", Encode.string someText )
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


dataFormatDecoder : Decode.Decoder DataFormat
dataFormatDecoder =
    Decode.string |> Decode.andThen createDataFormatDecoder


createDataFormatDecoder : String -> Decode.Decoder DataFormat
createDataFormatDecoder encodedFormat =
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
            Err "Invalid ModalContent Encoding"



-- UPDATE


type Msg
    = RemoveTag String
    | ToggleLocale
    | SetLocale String
    | TagInput String
    | CreateTagFromBuffer
    | FileSelected
    | ParseToCsv FileData
    | MapRecordToTag (Bucket Row) Tag
    | SearchPatternInput ColumnHeadingName SearchPattern
    | SetTaggingOption TaggingOption
    | NoOp
    | CloseModal
    | UndoMapRecordToTag UndoStrategy
    | TableDownload TableDataTagged
    | SelectMatchingRecords (List ColumnHeadingName) Tag (List Row)
    | SortTaggedTable Tag ColumnHeadingName


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
        SetLocale val ->
            case val of
                "en-EN" ->
                    ( updateSetLocale getEnglishLocale model, Cmd.none )

                "de-DE" ->
                    ( updateSetLocale getGermanLocale model, Cmd.none )

                _ ->
                    ( updateSetLocale getEnglishLocale model, Cmd.none )

        ToggleLocale ->
            if isEnglishLocale model.locale then
                ( updateSetLocale getGermanLocale model, Cmd.none )

            else if isGermanLocale model.locale then
                ( updateSetLocale getEnglishLocale model, Cmd.none )

            else
                ( updateSetLocale getDefaultLocale model, Cmd.none )

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
                                    createTableDataFromCsv csv model |> updateEvaluateDataFormats

                                Err _ ->
                                    case parseCsvString ',' decodedData of
                                        Ok csv ->
                                            createTableDataFromCsv csv model |> updateEvaluateDataFormats

                                        Err _ ->
                                            updateShowModalInfo (translateErrorHeading model.locale) (ViewInfo <| translateErrorParsingYourFile model.locale) model
                    in
                    ( newModel
                    , Cmd.none
                    )

                Err error ->
                    ( updateShowModalInfo (translateErrorHeading model.locale) (ViewInfo <| "There was an error reading your file : " ++ error) model, Cmd.none )

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
                    setCSVSemicolonsInList (translateTag model.locale :: headers)

                preparedRows =
                    List.map setCSVSemicolonsInList <| flattenRows rows

                theTable =
                    preparedHeaders
                        :: preparedRows

                theCsvString =
                    List.foldr String.append "" <| List.map (\row -> List.foldr String.append "" row) theTable
            in
            ( model, Download.string (tag ++ "-" ++ translateTableFileName model.locale ++ ".csv") "text/csv" theCsvString )

        SelectMatchingRecords headers tag rows ->
            let
                columnSearchPatternCount =
                    Dict.size model.batchTaggingOptions

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
                                columnSearchPatternCount > 0 && columnSearchPatternCount == matchingCellCount
                            )

                matchedRowsAsRowType =
                    matchedRows
                        |> List.map mapRowCellsToRemoveColumns

                plainMatchedRecords =
                    rowPlain matchedRowsAsRowType

                modalTitleText =
                    translateRecordsThatWillBeTagged model.locale (List.length plainMatchedRecords)

                modalDisplay =
                    if List.isEmpty plainMatchedRecords then
                        Data.Modal.RegularView

                    else
                        Data.Modal.Fullscreen
            in
            ( updateShowModal
                modalDisplay
                modalTitleText
                (ViewMapRecordsToTag headers plainMatchedRecords tag)
                model
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
                                comparison =
                                    case Dict.get column model.dataFormats of
                                        Just dataFormat ->
                                            case dataFormat of
                                                Text ->
                                                    Nothing

                                                Float ->
                                                    Just (compareWithParser parseFloat)

                                                Integer ->
                                                    Just (compareWithParser parseInt)

                                                Date ->
                                                    if isEnglishLocale model.locale then
                                                        Nothing

                                                    else
                                                        Just compareEuropeanDate

                                                Currency currency ->
                                                    Nothing

                                        Nothing ->
                                            Nothing

                                sortedRows =
                                    sort2dListByColumnWith colummnIndex comparison (rowPlain rows)
                                        |> List.map Row

                                newTableDataTagged =
                                    TableDataTagged tag headers sortedRows

                                newTableDataTaggedList =
                                    ListExtra.setAt theIndexCurrentTableDataTaggedByTag newTableDataTagged currentTableDataTaggedList
                            in
                            ( { model | tableDataTagged = List.append [ newTableDataTaggedList ] model.tableDataTagged }, Cmd.none )

                        Nothing ->
                            ( updateShowModalInfo "Sorting Tables" (ViewInfo "Index for TableData.Header lookup failed.") model, Cmd.none )

                _ ->
                    ( updateShowModalInfo "Sorting Tables" (ViewInfo "TableData lookup failed.") model, Cmd.none )


updateSetLocale : Locale -> Model -> Model
updateSetLocale locale model =
    { model | locale = locale }


updateSetDefaultTags : Locale -> Model -> Model
updateSetDefaultTags locale model =
    { model | tags = Set.fromList <| translateDefaultTags locale }


updateEvaluateDataFormats model =
    case getOriginalTableData model of
        Just tableData ->
            List.indexedMap
                (\colIndex column ->
                    case getColumnDataWithParser parseFloat colIndex tableData.rows of
                        Just floatList ->
                            updateDataFormat column Float

                        Nothing ->
                            case getColumnDataWithParser parseInt colIndex tableData.rows of
                                Just intList ->
                                    updateDataFormat column Integer

                                Nothing ->
                                    if isGermanLocale model.locale then
                                        case getColumnDataWithParser europeanDateToPosixParser colIndex tableData.rows of
                                            Just posixList ->
                                                updateDataFormat column Date

                                            Nothing ->
                                                updateDataFormat column Text

                                    else
                                        updateDataFormat column Text
                )
                tableData.headers
                |> List.foldl (\updatePart newModel -> updatePart newModel) model

        Nothing ->
            updateShowModalInfo (translateErrorHeading model.locale) (ViewInfo "Table Data lookup failed. No Data given.") model


updateDataFormat : ColumnHeadingName -> DataFormat -> Model -> Model
updateDataFormat column format model =
    { model | dataFormats = Dict.insert column format model.dataFormats }


updateShowModalInfo : Data.Modal.Title -> ModalContent -> Model -> Model
updateShowModalInfo title content model =
    { model | showModal = Just (Data.Modal.State content title Data.Modal.RegularView) }


updateShowModal : Data.Modal.DisplayProperties -> Data.Modal.Title -> ModalContent -> Model -> Model
updateShowModal displayProperties title content model =
    { model | showModal = Just (Data.Modal.State content title displayProperties) }


updateCloseModal : Model -> Model
updateCloseModal model =
    { model | showModal = Nothing }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileContentRead ParseToCsv
        , getLocale SetLocale
        ]



-- VIEW


viewModalContent : Locale -> ModalContent -> Html.Html Msg
viewModalContent locale modalContent =
    case modalContent of
        ViewMapRecordsToTag headers plainRecords tag ->
            if List.isEmpty plainRecords then
                text <| translateNoMatchingRecordsFound locale

            else
                Table.view (List.map (\column -> ( column, NoOp )) headers) <| List.map (List.map text) plainRecords

        ViewInfo info ->
            text info


getModalButtons : Locale -> ModalContent -> List (Data.Modal.Button Msg)
getModalButtons locale modalContent =
    case modalContent of
        ViewMapRecordsToTag headers plainRecords tag ->
            if List.isEmpty plainRecords then
                [ ( Data.Button.Secondary, CloseModal, "OK" )
                ]

            else
                let
                    saveMsg =
                        MapRecordToTag (Multiple <| List.map Row plainRecords) tag
                in
                [ ( Data.Button.Primary, saveMsg, translateSave locale )
                , ( Data.Button.Secondary, CloseModal, translateCancel locale )
                ]

        ViewInfo info ->
            [ ( Data.Button.Secondary, CloseModal, "OK" )
            ]


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

        modal =
            case model.showModal of
                Just modal_ ->
                    Modal.view
                        modal_.displayProperties
                        CloseModal
                        modal_.title
                        (viewModalContent model.locale modal_.content)
                        (getModalButtons model.locale modal_.content)

                Nothing ->
                    text ""

        localeTranslation =
            if isEnglishLocale model.locale then
                "EN"

            else if isGermanLocale model.locale then
                "DE"

            else
                "UNKNOWN"
    in
    div [ id "container", class "uk-container" ]
        [ modal
        , div
            []
            [ div [ class "uk-margin-top" ]
                [ button [ class "uk-button", onClick ToggleLocale ] [ text <| translateLocale model.locale ++ ": " ++ localeTranslation ] ]
            , div []
                [ Section.FileUpload.view (translateSelectAcsvFile model.locale) (maybeToBool model.file) FileSelected ]
            , div []
                [ Section.ManageTags.view (translateManageYourTags model.locale) (translateEnterATag model.locale) model.addTagInputError model.addTagInputBuffer model.tags TagInput CreateTagFromBuffer RemoveTag
                ]
            , div []
                [ viewTaggingSection
                    model.locale
                    model.optionTagging
                    model.batchTaggingOptions
                    model.tags
                    tableData.headers
                    currentRow
                    tableData.rows
                    taggingSectionNav
                ]
            ]
        , div
            [ class "row" ]
            [ div [ class "col-lg-12 col-sm-12" ]
                [ viewMappedRecordsPanel (translateTag model.locale) (translateTaggedRecords model.locale) tableData.headers tableDataTagged
                ]
            ]
        ]


viewTaggingSection : Locale -> TaggingOption -> Dict ColumnHeadingName SearchPattern -> Set Tag -> List ColumnHeadingName -> Row -> List Row -> HtmlNode -> HtmlNode
viewTaggingSection locale taggingOption batchTaggingOptions tags headers row rows nav =
    let
        translateHeaderText =
            translateApplyTags locale

        singleTaggingText =
            translateSingleTagging locale

        batchTaggingText =
            translateBatchTagging locale

        placeholderText =
            translateSelectAKeywordOrRegex locale

        tagActionText =
            translateSelectATagToTag locale

        helpText =
            translateHowBatchTaggingWorks locale

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
                    ( False, viewBatchTaggingTab placeholderText helpText batchTaggingOptions SearchPatternInput headers rows )
    in
    div []
        [ div [ class "uk-position-relative" ]
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span [ class "uk-text-background uk-text-large" ]
                    [ text (translateHeaderText (List.length rows))
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
                        [ a [ href "#" ] [ text singleTaggingText ] ]
                    , li
                        [ onClick (SetTaggingOption BatchTagging)
                        , classList [ ( "uk-active", not singleIsActiveTab ) ]
                        ]
                        [ a [ href "#" ] [ text batchTaggingText ] ]
                    ]
                ]
            , viewTab
            , div [ class "uk-margin" ] [ h5 [ class "uk-text-primary" ] [ text tagActionText ] ]
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


viewMappedRecordsPanel : String -> String -> List String -> List TableDataTagged -> Html Msg
viewMappedRecordsPanel tagTranslation taggedRecordsText headers_ someTables =
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
                                    |> List.append [ ( tagTranslation, NoOp ) ]
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
                    [ text taggedRecordsText ]
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


sort2dListByColumnWith : Int -> Maybe Comparison -> List (List String) -> List (List String)
sort2dListByColumnWith index comparison the2dList =
    List.sortWith (compareTwoListsByIndexWith index comparison) the2dList


compareTwoListsByIndexWith : Int -> Maybe Comparison -> List String -> List String -> Order
compareTwoListsByIndexWith index comparison firstList lastList =
    let
        compare_ =
            Maybe.withDefault compare comparison
    in
    case ( ListExtra.getAt index firstList, ListExtra.getAt index lastList ) of
        ( Just firstItem, Just nextItem ) ->
            compare_ firstItem nextItem

        ( Nothing, Just nextItem ) ->
            GT

        ( Just firstItem, Nothing ) ->
            LT

        ( Nothing, Nothing ) ->
            LT


compareWithParser : Parser.Parser comparable -> Comparison
compareWithParser parse first next =
    case ( Parser.run parse first, Parser.run parse next ) of
        ( Ok firstFloat, Ok nextFloat ) ->
            compare firstFloat nextFloat

        ( Err _, Ok nextFloat ) ->
            GT

        ( Ok firstFloat, Err _ ) ->
            LT

        ( Err _, Err _ ) ->
            LT


compareEuropeanDate : Comparison
compareEuropeanDate first next =
    case ( Parser.run europeanDateToPosixParser first |> Result.toMaybe, Parser.run europeanDateToPosixParser next |> Result.toMaybe ) of
        ( Just firstPosix, Just nextPosix ) ->
            Time.Extra.compare firstPosix nextPosix

        ( Nothing, Just nextFloat ) ->
            GT

        ( Just firstFloat, Nothing ) ->
            LT

        ( Nothing, Nothing ) ->
            LT


europeanDateToPosixParser : Parser.Parser Time.Posix
europeanDateToPosixParser =
    let
        separators =
            [ Parser.symbol "/", Parser.symbol "-", Parser.symbol ".", Parser.spaces ]
    in
    Parser.succeed (\day month year -> ( day, month, year ))
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.oneOf separators
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.oneOf separators
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.end
        |> Parser.andThen convertEuropeanDateToISO8601String
        |> Parser.andThen convertIso8601DateToPosix


convertIso8601DateToPosix : String -> Parser.Parser Time.Posix
convertIso8601DateToPosix date =
    {- UTC Zone is hard coded for now -}
    case Time.Extra.fromIso8601Date Time.utc date of
        Just aPosix ->
            Parser.succeed aPosix

        Nothing ->
            Parser.problem "Invalid ISO8601 Encoding"


convertEuropeanDateToISO8601String : ( String, String, String ) -> Parser.Parser String
convertEuropeanDateToISO8601String ( day, month, year ) =
    case ( String.toInt month, String.toInt day ) of
        ( Just monthVal, Just dayVal ) ->
            if monthVal > 12 || dayVal > 31 then
                Parser.problem <|
                    "invalid day or month value in date format; day is: "
                        ++ day
                        ++ " month is: "
                        ++ month

            else
                Parser.succeed <| year ++ "-" ++ month ++ "-" ++ day

        _ ->
            Parser.problem "invalid day or month value in date format"


parseInt : Parser.Parser Int
parseInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
            |. Parser.end
        , Parser.succeed identity
            |= Parser.int
            |. Parser.end
        ]


parseFloat : Parser.Parser Float
parseFloat =
    Parser.succeed (\sign left right -> ( sign, left, right ))
        |= Parser.oneOf
            [ Parser.map (always "-") (Parser.symbol "-")
            , Parser.succeed ""
            ]
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.oneOf
            [ Parser.symbol "."
            , Parser.symbol ","
            ]
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.end
        |> Parser.andThen convertToFloat


convertToFloat : ( String, String, String ) -> Parser.Parser Float
convertToFloat ( sign, first, last ) =
    case String.toFloat (sign ++ first ++ "." ++ last) of
        Just float ->
            Parser.succeed float

        Nothing ->
            Parser.problem "expecting float number"
