port module Main exposing (main)

import Browser
import Browser.Navigation
import Button
import Csv
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import File.Select
import Helpers
import Html exposing (Html, a, button, div, h1, h2, h3, h4, h5, hr, input, label, li, p, span, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, for, href, id, name, placeholder, style, target, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Lazy
import Input
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListExtra
import Locale exposing (Locale, translateImportData)
import Modal
import NavBar
import Parser
import Regex
import Set exposing (Set)
import Table as Table exposing (Cell, ColumnHeadingName, Row, TableData, TableDataTagged, Tag, decodeTableData, decodeTableDataList, decodeTableDataTaggedList, encodeTableData, encodeTableDataTagged, flattenRows, parseCurrencyToFloat, prependCellToRow)
import Task



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


port deleteStorage : () -> Cmd msg


port setStorage : Encode.Value -> Cmd msg


port getLocale : (String -> msg) -> Sub msg



-- TYPES


type alias HtmlNode =
    Html.Html Msg


type alias SearchPattern =
    String


type alias HtmlNodeId =
    String


type TaggingOption
    = SingleTagging
    | BatchTagging


type UndoStrategy
    = DropLast


type ModalContent
    = ViewImportFileRecords (List ColumnHeadingName) (List (List String)) ImportStacking
    | ViewMapRecordsToTag (List ColumnHeadingName) (List (List String)) Tag
    | ViewInfo String
    | ViewWarningDeleteLocalData
    | ViewDropIrregularRecords (List ColumnHeadingName) (List (List String)) (List (List String))
    | ViewIncompatibleData (List ColumnHeadingName) (List ColumnHeadingName)
    | ViewWorkingData (List ColumnHeadingName) (List (List String))
    | ViewManageTags
    | ViewTaggedData


type ImportStacking
    = Stack
    | Replace


type Workspace
    = Workspace (List ColumnHeadingName)
    | EmptyWorkspace


type Regex
    = IntegerRegex
    | PeriodFloatRegex
    | CommaFloatRegex


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
    , tableData : List TableData
    , tableDataTagged : List (List TableDataTagged)
    , batchTaggingOptions : Dict ColumnHeadingName SearchPattern
    , taggingMode : TaggingOption
    , showModal : Maybe (Modal.State ModalContent)
    , settingStackImportedData : ImportStacking
    , selectedWorkingData : TableData
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        locale =
            Result.withDefault Locale.getDefaultLocale <| Decode.decodeValue (Decode.field "locale" Locale.localeDecoder) flags

        tags =
            case Decode.decodeValue (Decode.field "tags" (Decode.list Decode.string)) flags of
                Ok val ->
                    Set.fromList val

                Err _ ->
                    Set.fromList (Locale.translateDefaultTags locale)

        addTagInputBuffer =
            Result.withDefault "" <| Decode.decodeValue (Decode.field "addTagInputBuffer" Decode.string) flags

        addTagInputError =
            Result.withDefault ( "", False ) <|
                Decode.decodeValue
                    (Decode.field "addTagInputError" (Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.bool)))
                    flags

        fileUploadPointerId =
            Result.withDefault "csv-upload" <| Decode.decodeValue (Decode.field "fileUploadPointerId" Decode.string) flags

        tableData =
            Result.withDefault [] <| decodeTableDataList flags "tableData"

        tableDataTagged =
            Result.withDefault [] <| decodeTableDataTaggedList flags "tableDataTagged"

        batchTaggingOptions =
            Result.withDefault Dict.empty <| Decode.decodeValue (Decode.field "batchTaggingOptions" (Decode.dict Decode.string)) flags

        showModal =
            Result.withDefault Nothing <| Decode.decodeValue (Decode.field "showModal" (Decode.nullable (Modal.createStateDecoder modalContentDecoder))) flags

        settingStackImportedData =
            Result.withDefault Stack <| Decode.decodeValue (Decode.field "settingStackImportedData" (Decode.string |> Decode.andThen decodeImportStacking)) flags

        selectedWorkingData =
            Result.withDefault (TableData [] []) <| Decode.decodeValue (Decode.field "selectedWorkingData" decodeTableData) flags
    in
    ( { locale = locale
      , tags = tags
      , addTagInputBuffer = addTagInputBuffer
      , addTagInputError = addTagInputError
      , fileUploadPointerId = fileUploadPointerId
      , tableData = tableData
      , tableDataTagged = tableDataTagged
      , batchTaggingOptions = batchTaggingOptions
      , taggingMode = BatchTagging
      , showModal = showModal
      , settingStackImportedData = settingStackImportedData
      , selectedWorkingData = selectedWorkingData
      }
    , Cmd.none
    )


tuple2Encoder : (a -> Encode.Value) -> (b -> Encode.Value) -> ( a, b ) -> Encode.Value
tuple2Encoder enc1 enc2 ( val1, val2 ) =
    Encode.list identity [ enc1 val1, enc2 val2 ]


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "locale", Locale.encodeLocale model.locale )
        , ( "tags", Encode.set Encode.string model.tags )
        , ( "addTagInputBuffer", Encode.string model.addTagInputBuffer )
        , ( "addTagInputError", tuple2Encoder Encode.string Encode.bool model.addTagInputError )
        , ( "fileUploadPointerId", Encode.string model.fileUploadPointerId )
        , ( "tableData", Encode.list encodeTableData model.tableData )
        , ( "tableDataTagged", Encode.list (Encode.list encodeTableDataTagged) model.tableDataTagged )
        , ( "batchTaggingOptions", Encode.dict identity Encode.string model.batchTaggingOptions )
        , ( "showModal", encodeShowModal model.showModal )
        , ( "settingStackImportedData", encodeImportStacking model.settingStackImportedData )
        , ( "selectedWorkingData", encodeTableData model.selectedWorkingData )
        ]


decodeImportStacking : String -> Decode.Decoder ImportStacking
decodeImportStacking encoded =
    case parseImportStacking encoded of
        Ok stackingType ->
            Decode.succeed stackingType

        Err err ->
            Decode.fail err


parseImportStacking : String -> Result String ImportStacking
parseImportStacking val =
    case val of
        "stack" ->
            Ok Stack

        "replace" ->
            Ok Replace

        _ ->
            Err "Invalid settingStackImportedData encoding"


encodeImportStacking : ImportStacking -> Encode.Value
encodeImportStacking importStacking =
    case importStacking of
        Stack ->
            Encode.string "stack"

        Replace ->
            Encode.string "replace"


encodeShowModal : Maybe (Modal.State ModalContent) -> Encode.Value
encodeShowModal showModal =
    case showModal of
        Just modalState ->
            Modal.encodeState modalState encodeModalContent

        Nothing ->
            Encode.null


modalContentDecoder : Decode.Decoder ModalContent
modalContentDecoder =
    Decode.oneOf
        [ Decode.field "viewTaggedData" (Decode.succeed ViewTaggedData)
        , Decode.field "viewManageTags" (Decode.succeed ViewManageTags)
        , Decode.field "viewWorkingData"
            (Decode.map2 ViewWorkingData
                (Decode.field "columns" <| Decode.list Decode.string)
                (Decode.field "records" <| Decode.list (Decode.list Decode.string))
            )
        , Decode.field "viewImportFileRecords"
            (Decode.map3 ViewImportFileRecords
                (Decode.field "columns" <| Decode.list Decode.string)
                (Decode.field "records" <| Decode.list (Decode.list Decode.string))
                (Decode.field "stacking" <| (Decode.string |> Decode.andThen decodeImportStacking))
            )
        , Decode.field "viewMapRecordsToTag"
            (Decode.map3 ViewMapRecordsToTag
                (Decode.field "columns" <| Decode.list Decode.string)
                (Decode.field "records" <| Decode.list (Decode.list Decode.string))
                (Decode.field "tag" Decode.string)
            )
        , Decode.field "viewInfo" (Decode.string |> Decode.map ViewInfo)
        , Decode.field "viewWarningDeleteLocalData" (Decode.succeed ViewWarningDeleteLocalData)
        , Decode.field "viewDropIrregularRecords"
            (Decode.map3 ViewDropIrregularRecords
                (Decode.field "columns" <| Decode.list Decode.string)
                (Decode.field "irregularRecords" <| Decode.list (Decode.list Decode.string))
                (Decode.field "regularRecords" <| Decode.list (Decode.list Decode.string))
            )
        , Decode.field "viewIncompatibleData"
            (Decode.map2 ViewIncompatibleData
                (Decode.field "compatibleHeaders" <| Decode.list Decode.string)
                (Decode.field "incompatibleHeaders" <| Decode.list Decode.string)
            )
        ]


encodeModalContent : ModalContent -> Encode.Value
encodeModalContent modalContent_ =
    case modalContent_ of
        ViewTaggedData ->
            Encode.object
                [ ( "viewTaggedData", Encode.null )
                ]

        ViewManageTags ->
            Encode.object
                [ ( "viewManageTags", Encode.null )
                ]

        ViewWorkingData columns records ->
            let
                valuesEncoding =
                    Encode.object
                        [ ( "columns", Encode.list Encode.string columns )
                        , ( "records", Encode.list (Encode.list Encode.string) records )
                        ]
            in
            Encode.object
                [ ( "viewWorkingData", valuesEncoding )
                ]

        ViewImportFileRecords columns records stacking ->
            let
                valuesEncoding =
                    Encode.object
                        [ ( "stacking", encodeImportStacking stacking )
                        , ( "columns", Encode.list Encode.string columns )
                        , ( "records", Encode.list (Encode.list Encode.string) records )
                        ]
            in
            Encode.object
                [ ( "viewImportFileRecords", valuesEncoding )
                ]

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

        ViewWarningDeleteLocalData ->
            Encode.object
                [ ( "viewWarningDeleteLocalData", Encode.null )
                ]

        ViewDropIrregularRecords columns irregularRecords regularRecords ->
            let
                valuesEncoding =
                    Encode.object
                        [ ( "columns", Encode.list Encode.string columns )
                        , ( "regularRecords", Encode.list (Encode.list Encode.string) regularRecords )
                        , ( "irregularRecords", Encode.list (Encode.list Encode.string) irregularRecords )
                        ]
            in
            Encode.object
                [ ( "viewDropIrregularRecords", valuesEncoding )
                ]

        ViewIncompatibleData compatibleHeaders incompatibleHeaders ->
            let
                valuesEncoding =
                    Encode.object
                        [ ( "compatibleHeaders", Encode.list Encode.string compatibleHeaders )
                        , ( "incompatibleHeaders", Encode.list Encode.string incompatibleHeaders )
                        ]
            in
            Encode.object
                [ ( "viewIncompatibleData", valuesEncoding )
                ]



-- UPDATE


{-| Naming Schema : SomeoneDidSomethingSomewhereAndSomeHow
-}
type Msg
    = RemoveTag String
    | UserClickedViewTaggedDataButton
    | UserClickedManageTagsButton
    | UserClickedItemInPlaceRegexDropDownList ColumnHeadingName Regex
    | UserClickedRequestDeleteDataButton
    | DeleteLocalData
    | UserClickedToggleLocale
    | SetLocale String
    | TagInput String
    | CreateTagFromBuffer
    | MapRecordToTag (Bucket Row) Tag
    | SearchPatternInput ColumnHeadingName SearchPattern
    | SetTaggingMode TaggingOption
    | NoOp
    | CloseModal
    | UndoMapRecordToTag UndoStrategy
    | TableDownload TableDataTagged
    | SortTaggedTable Tag ColumnHeadingName
    | UserClickedInitialFileSelectButton
    | UserSelectedFileFromSysDialog File
    | RuntimeCompletedFileLoadingTask String
    | UserClickedStackingCheckboxInImportDialog (List ColumnHeadingName) (List (List String)) ImportStacking
    | UserClickedConfirmDataImportButton ImportStacking (List ColumnHeadingName) (List (List String))
    | UserClickedDropButtonInDropDialog (List ColumnHeadingName) (List (List String)) (List (List String))
    | UserClickedViewWorkingDataNavButtonInTaggingSection
    | UserClickedImportFileButton


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
        UserClickedViewTaggedDataButton ->
            ( updateShowModal Modal.Fullscreen (Locale.translateTaggedRecords model.locale) ViewTaggedData model, Cmd.none )

        UserClickedManageTagsButton ->
            ( updateShowModal Modal.RegularView (Locale.translateManageYourTags model.locale) ViewManageTags model, Cmd.none )

        UserClickedImportFileButton ->
            ( model, File.Select.file [ "text/csv" ] UserSelectedFileFromSysDialog )

        UserClickedViewWorkingDataNavButtonInTaggingSection ->
            let
                { headers, rows } =
                    Maybe.withDefault (Table.TableData [] []) (getWorkingData model)
            in
            ( updateShowModal Modal.Fullscreen (Locale.translateTitleRemainingWorkingData model.locale) (ViewWorkingData headers (Table.rowPlain rows)) model, Cmd.none )

        UserClickedItemInPlaceRegexDropDownList column regex ->
            let
                regexStr =
                    case regex of
                        IntegerRegex ->
                            "^[-+]?\\d+$"

                        PeriodFloatRegex ->
                            "^[-+]?[0-9]+\\.[0-9]+$"

                        CommaFloatRegex ->
                            "^[-+]?[0-9]+\\,[0-9]+$"
            in
            ( { model | batchTaggingOptions = Dict.insert column regexStr model.batchTaggingOptions }, Cmd.none )

        UserClickedDropButtonInDropDialog headers _ regularRecords ->
            ( updateShowImportConfirmation headers regularRecords model.settingStackImportedData model, Cmd.none )

        UserClickedStackingCheckboxInImportDialog headers records importStacking ->
            case importStacking of
                Stack ->
                    ( updateShowImportConfirmation headers records Replace model, Cmd.none )

                Replace ->
                    ( updateShowImportConfirmation headers records Stack model, Cmd.none )

        UserClickedConfirmDataImportButton stacking headers records ->
            ( updateCloseModal <| createTableDataFromCsv stacking (Csv.Csv headers records) model, Cmd.none )

        RuntimeCompletedFileLoadingTask content ->
            let
                commaParsedCsv =
                    parseCsvString ',' content

                semicolonParsedCsv =
                    parseCsvString ';' content

                noRecords =
                    List.length semicolonParsedCsv.records == 0 && List.length commaParsedCsv.records == 0

                parsedCsv =
                    if List.length semicolonParsedCsv.headers > List.length commaParsedCsv.headers then
                        semicolonParsedCsv

                    else
                        commaParsedCsv

                ( irregularRecords, regularRecords ) =
                    partitionIrregularRowsInLength parsedCsv.headers parsedCsv.records

                checkForIrregularityOrProceed =
                    if List.length irregularRecords > 0 then
                        updateShowModal
                            Modal.Fullscreen
                            (Locale.translateIrregularRowsTitle model.locale)
                            (ViewDropIrregularRecords parsedCsv.headers irregularRecords regularRecords)
                            model

                    else
                        updateShowImportConfirmation parsedCsv.headers regularRecords model.settingStackImportedData model

                incompatibleDataDialog compatibleHeaders incompatibleHeaders model_ =
                    updateShowModalInfo (Locale.translateIncompatibleDataTitle model_.locale) (ViewIncompatibleData compatibleHeaders incompatibleHeaders) model_

                workspace =
                    getWorkspace model

                newModel =
                    if String.isEmpty content then
                        updateShowModalInfo (Locale.translateEmptyFileTitle model.locale) (ViewInfo <| Locale.translateEmptyFileText model.locale) model

                    else if noRecords then
                        updateShowModalInfo
                            (Locale.translateImportDataNoRecordsFoundTitle model.locale)
                            (ViewInfo <| Locale.translateImportDataNoRecordsFound model.locale)
                            model

                    else
                        case workspace of
                            Workspace workspaceHeaders ->
                                if workspaceHeaders /= parsedCsv.headers then
                                    incompatibleDataDialog workspaceHeaders parsedCsv.headers model

                                else
                                    checkForIrregularityOrProceed

                            EmptyWorkspace ->
                                checkForIrregularityOrProceed
            in
            ( newModel
            , Cmd.none
            )

        UserClickedInitialFileSelectButton ->
            ( model, File.Select.file [ "text/csv" ] UserSelectedFileFromSysDialog )

        UserSelectedFileFromSysDialog file ->
            ( model, Task.perform RuntimeCompletedFileLoadingTask (File.toString file) )

        UserClickedRequestDeleteDataButton ->
            ( updateShowModal Modal.RegularView (Locale.translateTitleDeleteLocalData model.locale) ViewWarningDeleteLocalData model
            , Cmd.none
            )

        DeleteLocalData ->
            ( updateCloseModal model, Cmd.batch [ deleteStorage (), Browser.Navigation.reload ] )

        SetLocale val ->
            case val of
                "en-EN" ->
                    ( updateSetLocale Locale.getEnglishLocale model, Cmd.none )

                "de-DE" ->
                    ( updateSetLocale Locale.getGermanLocale model, Cmd.none )

                _ ->
                    ( updateSetLocale Locale.getEnglishLocale model, Cmd.none )

        UserClickedToggleLocale ->
            if Locale.isEnglishLocale model.locale then
                ( updateSetLocale Locale.getGermanLocale model, Cmd.none )

            else if Locale.isGermanLocale model.locale then
                ( updateSetLocale Locale.getEnglishLocale model, Cmd.none )

            else
                ( updateSetLocale Locale.getDefaultLocale model, Cmd.none )

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
                                        Table.detectDataFormats (TableDataTagged theTag table.headers updatedRows table.dataFormats)

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
            let
                updatedBatchTaggingOptions =
                    if String.isEmpty searchPatternInput then
                        Dict.remove columnKey model.batchTaggingOptions

                    else
                        Dict.insert columnKey searchPatternInput model.batchTaggingOptions

                workingTableData =
                    Maybe.withDefault (Table.TableData [] []) (getWorkingData model)

                headers =
                    workingTableData.headers

                rows =
                    workingTableData.rows

                matchedRows =
                    matchRows updatedBatchTaggingOptions headers rows

                modelWithUpdatedSelectedWorkingData =
                    if Dict.isEmpty updatedBatchTaggingOptions then
                        workingTableData

                    else
                        TableData headers matchedRows
            in
            ( { model | batchTaggingOptions = updatedBatchTaggingOptions, selectedWorkingData = modelWithUpdatedSelectedWorkingData }, Cmd.none )

        SetTaggingMode opt ->
            ( { model | taggingMode = opt }, Cmd.none )

        CloseModal ->
            ( updateCloseModal model, Cmd.none )

        UndoMapRecordToTag undoStrategy ->
            case undoStrategy of
                DropLast ->
                    let
                        ( newHistoryData, newHistoryDataTagged ) =
                            case ( model.tableData, model.tableDataTagged ) of
                                ( _ :: restTableD, _ :: restTableDT ) ->
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
                    setCSVSemicolonsInList (Locale.translateTag model.locale :: headers)

                preparedRows =
                    List.map setCSVSemicolonsInList <| flattenRows rows

                theTable =
                    preparedHeaders
                        :: preparedRows

                theCsvString =
                    List.foldr String.append "" <| List.map (\row -> List.foldr String.append "" row) theTable
            in
            ( model, Download.string (tag ++ "-" ++ Locale.translateTableFileName model.locale ++ ".csv") "text/csv" theCsvString )

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
                ( Just tableData, Just theIndexCurrentTableDataTaggedByTag ) ->
                    case ListExtra.elemIndex column tableData.headers of
                        Just columnIndex ->
                            let
                                newTableDataTagged =
                                    Table.sort column columnIndex tableData

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


updateShowImportConfirmation : List ColumnHeadingName -> List (List String) -> ImportStacking -> Model -> Model
updateShowImportConfirmation headers records stackSetting model =
    updateShowModal
        Modal.Fullscreen
        (translateImportData model.locale)
        (ViewImportFileRecords headers records stackSetting)
        model


updateShowModalInfo : Modal.Title -> ModalContent -> Model -> Model
updateShowModalInfo title content model =
    { model | showModal = Just (Modal.State content title Modal.RegularView) }


updateShowModal : Modal.DisplayProperties -> Modal.Title -> ModalContent -> Model -> Model
updateShowModal displayProperties title content model =
    { model | showModal = Just (Modal.State content title displayProperties) }


updateCloseModal : Model -> Model
updateCloseModal model =
    { model | showModal = Nothing }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ getLocale SetLocale
        ]



-- VIEW


viewRecords : Table.Responsive -> List ColumnHeadingName -> List (List String) -> Html.Html Msg
viewRecords responsive headers records =
    Table.view
        responsive
        (List.map (\column -> ( column, NoOp )) headers)
        (List.map (List.map text) records)


viewRows : Table.Responsive -> List ColumnHeadingName -> List Row -> Html.Html Msg
viewRows responsive headers rows =
    viewRecords responsive headers <| unwrapRows rows


viewModalContent : Locale -> Model -> ModalContent -> Html.Html Msg
viewModalContent locale model modalContent =
    case modalContent of
        ViewWorkingData headers records ->
            if List.isEmpty records then
                text <| Locale.translateNoWorkingData locale

            else
                viewRecords Table.Responsive headers records

        ViewManageTags ->
            div []
                [ Input.viewWithButton
                    [ onInput TagInput, value model.addTagInputBuffer, placeholder (Locale.translateEnterATag model.locale) ]
                    Button.Add
                    CreateTagFromBuffer
                , viewTagList
                    (\tag -> RemoveTag tag)
                    model.tags
                ]

        ViewTaggedData ->
            let
                someTables =
                    Maybe.withDefault [] (List.head model.tableDataTagged)

                headers_ =
                    case getWorkspace model of
                        EmptyWorkspace ->
                            []

                        Workspace headers ->
                            headers

                preparedRows : List { tag : Tag, headers : List ( ColumnHeadingName, Msg ), rows : List Row, dataFormats : Dict ColumnHeadingName Table.DataFormat }
                preparedRows =
                    List.map
                        (\{ tag, headers, rows, dataFormats } ->
                            let
                                headersWithSortMsg =
                                    List.map (\column -> ( column, SortTaggedTable tag column )) headers
                                        |> List.append [ ( Locale.translateTag model.locale, NoOp ) ]
                            in
                            { tag = tag, headers = headersWithSortMsg, rows = List.map (prependCellToRow tag) rows, dataFormats = dataFormats }
                        )
                        someTables

                rowsViews =
                    preparedRows
                        |> List.map
                            (\tableDataTagged ->
                                Table.viewWithTagData
                                    Table.Responsive
                                    -- use original header list for Tabledownload, since we modified it before
                                    (TableDownload <| TableDataTagged tableDataTagged.tag headers_ tableDataTagged.rows tableDataTagged.dataFormats)
                                    tableDataTagged
                            )
            in
            div []
                rowsViews

        ViewImportFileRecords headers records stacking ->
            if List.isEmpty records then
                text <| Locale.translateImportDataNoRecordsFound locale

            else
                let
                    isChecked =
                        case stacking of
                            Stack ->
                                True

                            Replace ->
                                False
                in
                Html.div
                    [ Html.Attributes.style "position" "relative" ]
                    [ div [ Html.Attributes.style "position" "fixed", Html.Attributes.style "bottom" "10px" ]
                        [ Html.label [ onClick (UserClickedStackingCheckboxInImportDialog headers records stacking) ]
                            [ input [ type_ "checkbox", checked isChecked, class "uk-checkbox" ] []
                            , text <| " " ++ Locale.translateCheckboxStackData locale
                            ]
                        , div
                            []
                            [ span
                                [ class "uk-label uk-label-warning uk-text-small uk-margin-left", style "vertical-align" "text-bottom" ]
                                [ text <| Locale.translateWarningLabel locale ]
                            , span [ class "uk-text-small uk-text-light" ] [ text <| "   " ++ Locale.translateUncheckStackDataWarning locale ]
                            ]
                        ]
                    , viewRecords Table.Responsive headers records
                    ]

        ViewMapRecordsToTag headers plainRecords _ ->
            if List.isEmpty plainRecords then
                text <| Locale.translateNoMatchingRecordsFound locale

            else
                viewRecords Table.Responsive headers plainRecords

        ViewInfo info ->
            text info

        ViewWarningDeleteLocalData ->
            text (Locale.translateWarningDeleteLocalData locale)

        ViewDropIrregularRecords headers irregularRecords _ ->
            div
                []
                [ h5 [] [ text <| Locale.translateIrregularRowsText locale (List.length irregularRecords) ++ " :" ]
                , viewRecords Table.Unresponsive headers irregularRecords
                , h5 [] [ text <| Locale.translateAskForDrop locale ]
                ]

        ViewIncompatibleData validHeaders unvalidHeaders ->
            let
                validHeadersWithSemicolons =
                    setCSVSemicolonsInList validHeaders

                unValidHeadersWithSemicolons =
                    setCSVSemicolonsInList unvalidHeaders

                viewColumns textClass headerText =
                    String.toList headerText
                        |> List.foldl
                            (\char result ->
                                if char == ' ' then
                                    List.append result [ span [ style "borderBottom" "1px dotted blue" ] [ text "   " ] ]

                                else
                                    List.append result [ span [ class textClass ] [ text <| String.fromChar char ] ]
                            )
                            []
            in
            div
                []
                [ h5 [] [ text <| Locale.translateIncompatibleDataIntro locale ]
                , div [ style "whiteSpace" "pre" ]
                    (validHeadersWithSemicolons
                        |> List.map (viewColumns "uk-text-success")
                        |> List.concat
                    )
                , h5 [] [ text <| Locale.translateIncompatibleDataComparison locale ]
                , div [ style "whiteSpace" "pre" ]
                    (unValidHeadersWithSemicolons
                        |> List.indexedMap
                            (\i header ->
                                case ListExtra.getAt i validHeadersWithSemicolons of
                                    Just validHeader ->
                                        if validHeader == header then
                                            viewColumns "uk-text-success" header

                                        else
                                            viewColumns "uk-text-danger" header

                                    Nothing ->
                                        viewColumns "uk-text-danger" header
                            )
                        |> List.concat
                    )
                ]


getModalButtons : Locale -> ModalContent -> List (Modal.Button Msg)
getModalButtons locale modalContent =
    case modalContent of
        ViewTaggedData ->
            [ Modal.DefaultButton Button.Secondary CloseModal (Locale.translateClose locale)
            ]

        ViewManageTags ->
            [ Modal.DefaultButton Button.Secondary CloseModal (Locale.translateClose locale)
            ]

        ViewWorkingData _ _ ->
            [ Modal.DefaultButton Button.Secondary CloseModal "Ok"
            ]

        ViewImportFileRecords headers records stacking ->
            if List.isEmpty records then
                [ Modal.DefaultButton Button.Secondary CloseModal (Locale.translateCancel locale)
                ]

            else
                [ Modal.IconButton Button.Primary (UserClickedConfirmDataImportButton stacking headers records) (Locale.translateImport locale) Button.Import
                , Modal.DefaultButton Button.Secondary CloseModal (Locale.translateCancel locale)
                ]

        ViewMapRecordsToTag _ plainRecords tag ->
            if List.isEmpty plainRecords then
                [ Modal.DefaultButton Button.Secondary CloseModal "Ok"
                ]

            else
                let
                    saveMsg =
                        MapRecordToTag (Multiple <| List.map Row plainRecords) tag
                in
                [ Modal.DefaultButton Button.Primary saveMsg (Locale.translateSave locale)
                , Modal.DefaultButton Button.Secondary CloseModal (Locale.translateCancel locale)
                ]

        ViewInfo _ ->
            [ Modal.DefaultButton Button.Secondary CloseModal "Ok"
            ]

        ViewWarningDeleteLocalData ->
            [ Modal.DefaultButton Button.Danger DeleteLocalData (Locale.translateProceed locale)
            , Modal.DefaultButton Button.Default CloseModal (Locale.translateCancel locale)
            ]

        ViewDropIrregularRecords headers irregularRecords regularRecords ->
            [ Modal.DefaultButton Button.Primary (UserClickedDropButtonInDropDialog headers irregularRecords regularRecords) (Locale.translateDrop locale)
            , Modal.DefaultButton Button.Secondary CloseModal (Locale.translateCancel locale)
            ]

        ViewIncompatibleData _ _ ->
            [ Modal.DefaultButton Button.Secondary CloseModal (Locale.translateCancel locale)
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
            viewMainNav ( model.tableData, model.tableDataTagged )

        modal =
            case model.showModal of
                Just modal_ ->
                    Modal.view
                        modal_.displayProperties
                        CloseModal
                        modal_.title
                        (viewModalContent model.locale model modal_.content)
                        (getModalButtons model.locale modal_.content)

                Nothing ->
                    text ""

        localeTranslation =
            if Locale.isEnglishLocale model.locale then
                "EN"

            else if Locale.isGermanLocale model.locale then
                "DE"

            else
                "UNKNOWN"
    in
    div [ id "container", class "uk-container" ]
        [ modal
        , div
            []
            [ div [ class "uk-text-center uk-margin-top" ]
                [ span [ attribute "uk-icon" "file-text", style "vertical-align" "text-bottom" ] []
                , a [ href "https://www.robingruenke.com/journal/documentation/tools/documentation-for-my-csv-batch-tagging-tool.html", class "uk-text-success", target "_blank" ] [ text <| " " ++ Locale.translateViewUserDocumentation model.locale ]
                , span [] [ text " | " ]
                , span [ attribute "uk-icon" "github", style "vertical-align" "text-bottom" ] []
                , a [ href "https://github.com/eimfach/elm-csv-batch-tagger-app", target "_blank" ] [ text <| " " ++ Locale.translateViewSourceCode model.locale ]
                ]
            , hr [] []
            , div []
                [ viewTaggingSection
                    model
                    tableData.headers
                    currentRow
                    tableData.rows
                    taggingSectionNav
                ]
            , div [ class "uk-margin" ]
                [ span
                    [ class "uk-label uk-text-small" ]
                    [ text "NOTE" ]
                , span [ class "uk-text-small uk-text-light" ] [ text <| "   " ++ Locale.translateInfoOnHowDataIsStored model.locale ]
                ]
            ]
        ]



-- VIEW TAGS


viewTagList : (Table.Tag -> msg) -> Set Table.Tag -> Html msg
viewTagList msg tags =
    ul [ class "uk-list uk-list-divider" ]
        (tags
            |> Set.toList
            |> List.map (viewTagListItem msg)
        )


viewTagListItem : (Table.Tag -> msg) -> Table.Tag -> Html msg
viewTagListItem msg tag =
    li []
        [ span
            []
            [ text tag ]
        , Button.view
            (msg tag)
            (Button.Composed Button.Default Button.Medium Button.Right)
            Button.Remove
            ""
        ]


viewTagButtons : (Table.Tag -> msg) -> Set Table.Tag -> List (Html msg)
viewTagButtons msg tags =
    tags
        |> Set.toList
        |> List.map (viewTagButton msg)


viewTagButton : (Table.Tag -> msg) -> Table.Tag -> Html msg
viewTagButton msg tag =
    Button.view
        (msg tag)
        (Button.Composed Button.Primary Button.Medium Button.NoAlign)
        Button.NoActionType
        tag



-- VIEW SECTIONS


viewInitialFileSelect : Html Msg
viewInitialFileSelect =
    div
        [ class "uk-padding uk-center uk-flex uk-flex-center" ]
        [ button
            [ onClick UserClickedInitialFileSelectButton
            , class "uk-button uk-button-primary uk-button-large uk-width-1-3"
            ]
            [ label
                [ attribute "uk-icon" "icon: plus"
                , for "csv-upload"
                , class "file-label"
                ]
                [ text "" ]
            ]
        ]


viewManageTagsSection : String -> String -> ( String, Bool ) -> String -> Set Table.Tag -> (Table.Tag -> msg) -> msg -> (Table.Tag -> msg) -> Html.Html msg
viewManageTagsSection headerText inputPlaceholder _ buffer tags tagInputMsg createTagMsg removeTagMsg =
    div []
        [ div []
            [ h3
                [ class "uk-heading-line uk-text-center" ]
                [ span [ class "uk-text-background uk-text-large" ] [ text headerText ]
                ]
            ]
        , div [ class "uk-padding" ]
            [ Input.viewWithButton
                [ onInput tagInputMsg, value buffer, placeholder inputPlaceholder ]
                Button.Add
                createTagMsg
            , viewTagList
                (\tag -> removeTagMsg tag)
                tags
            ]
        ]


viewTaggingSection : Model -> List ColumnHeadingName -> Row -> List Row -> HtmlNode -> HtmlNode
viewTaggingSection model headers row rows nav =
    let
        batchTaggingText =
            Locale.translateBatchTagging model.locale

        ( singleIsActiveTab, viewTab ) =
            case model.taggingMode of
                {--tab selection is naive--}
                SingleTagging ->
                    ( True, viewManualTaggingTab model.locale headers row.cells )

                BatchTagging ->
                    ( False, viewBatchTaggingTab model SearchPatternInput headers rows )
    in
    div [ class "uk-card uk-card-primary uk-card-body uk-width-2xlarge" ]
        [ nav
        , div [ class "uk-padding" ]
            [ div [ class "uk-width-1-1 uk-margin-large" ]
                [ ul
                    [ class "uk-child-width-expand", attribute "uk-tab" "" ]
                    [ li
                        [ onClick (SetTaggingMode BatchTagging)
                        , classList [ ( "uk-active", not singleIsActiveTab ) ]
                        ]
                        [ a [ href "#" ] [ text batchTaggingText ] ]
                    ]
                ]
            , viewTab
            ]
        ]


viewEmptyTabContent =
    [ viewInitialFileSelect
    ]


viewManualTaggingTab : Locale.Locale -> List ColumnHeadingName -> List String -> Html.Html Msg
viewManualTaggingTab locale columns records =
    let
        content =
            if List.isEmpty records then
                viewEmptyTabContent

            else
                [ Table.viewSingle
                    []
                    columns
                    (List.map text records)
                ]
    in
    div [] content


viewManualTaggingHelp locale =
    p
        [ class "uk-text-meta" ]
        [ span
            [ class "uk-label uk-text-small" ]
            [ text "NOTE" ]
        , text <| "  " ++ Locale.translateHowManualTaggingWorks locale
        ]


viewBatchTaggingTab : Model -> (ColumnHeadingName -> SearchPattern -> Msg) -> List ColumnHeadingName -> List Row -> Html.Html Msg
viewBatchTaggingTab model inputAction columns records =
    let
        instantSearchResults =
            if List.isEmpty model.selectedWorkingData.rows then
                div [ class "uk-flex uk-flex-center uk-flex-middle uk-padding uk-animation-shake" ]
                    [ div [ class "" ]
                        [ span [ class "uk-text-warning", attribute "uk-icon" "icon: search; ratio: 2" ] []
                        , span [ class "uk-text-middle uk-text-italic uk-text-warning" ] [ text "Couldn't find anything ..." ]
                        ]
                    ]

            else
                div []
                    [ NavBar.viewIconNav False
                        []
                        [ ( NavBar.CountBadge <| List.length model.selectedWorkingData.rows, NoOp, [] )
                        , ( NavBar.Export, NoOp, [] )
                        , ( NavBar.TagData, NoOp, [] )
                        ]
                    , viewRows Table.Unresponsive model.selectedWorkingData.headers model.selectedWorkingData.rows
                    ]

        content =
            if List.isEmpty records then
                viewEmptyTabContent

            else
                [ viewBatchTagging model.locale model.batchTaggingOptions inputAction columns records
                , hr [] []
                , instantSearchResults
                ]
    in
    div [] content


viewBatchTaggingHelp locale =
    p
        [ class "uk-text-meta" ]
        [ span
            [ class "uk-label uk-text-small" ]
            [ text "NOTE" ]
        , span [ class "uk-text-small uk-text-light" ] [ text <| "   " ++ Locale.translateHowBatchTaggingWorks locale ]
        ]


viewBatchTagging : Locale.Locale -> Dict ColumnHeadingName SearchPattern -> (ColumnHeadingName -> SearchPattern -> Msg) -> List ColumnHeadingName -> List Row -> Html.Html Msg
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
                                    Set.fromList <| Table.getColumnData index records
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


viewBatchTaggingInput : String -> Locale.Locale -> String -> String -> Set String -> (SearchPattern -> Msg) -> Html.Html Msg
viewBatchTaggingInput labelText locale idVal val options action =
    div [ class "float-button" ]
        [ Input.viewAutocomplete
            labelText
            "search"
            val
            idVal
            [ placeholder (Locale.translateSelectAKeywordOrRegex locale), onInput action ]
            options
        , div [ class "dropdown" ]
            [ Button.viewWithDropDown
                Button.Default
                Button.AddRegex
                "Place Regex"
              <|
                getRegexDropdown labelText
            ]
        ]


viewMainNav : ( List a, List a1 ) -> Html Msg
viewMainNav ( history1, history2 ) =
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
        True
        [ ( NavBar.Delete, UserClickedRequestDeleteDataButton, [] )
        , ( NavBar.Language, UserClickedToggleLocale, [] )
        , ( NavBar.Spacer, NoOp, [] )
        , undoButton
        , ( NavBar.Disabled NavBar.Workspace, NoOp, [] )

        -- , ( NavBar.Disabled NavBar.Redo, NoOp, [] )
        -- , ( NavBar.Disabled NavBar.Backward, NoOp, [] )
        -- , ( NavBar.Disabled NavBar.Forward, NoOp, [] )
        ]
        [ ( NavBar.Import, UserClickedImportFileButton, [] )
        , ( NavBar.Spacer, NoOp, [] )
        , ( NavBar.ViewTableData, UserClickedViewWorkingDataNavButtonInTaggingSection, [] )
        , ( NavBar.ViewTaggedData, UserClickedViewTaggedDataButton, [] )
        , ( NavBar.Spacer, NoOp, [] )
        , ( NavBar.ViewManageTags, UserClickedManageTagsButton, [] )
        ]


viewMappedRecordsPanel : String -> String -> List String -> List TableDataTagged -> Html Msg
viewMappedRecordsPanel tagTranslation taggedRecordsText headers_ someTables =
    if List.isEmpty someTables then
        text ""

    else
        let
            preparedRows : List { tag : Tag, headers : List ( ColumnHeadingName, Msg ), rows : List Row, dataFormats : Dict ColumnHeadingName Table.DataFormat }
            preparedRows =
                List.map
                    (\{ tag, headers, rows, dataFormats } ->
                        let
                            headersWithSortMsg =
                                List.map (\column -> ( column, SortTaggedTable tag column )) headers
                                    |> List.append [ ( tagTranslation, NoOp ) ]
                        in
                        { tag = tag, headers = headersWithSortMsg, rows = List.map (prependCellToRow tag) rows, dataFormats = dataFormats }
                    )
                    someTables

            rowsViews =
                preparedRows
                    |> List.map
                        (\tableDataTagged ->
                            Table.viewWithTagData
                                Table.Responsive
                                -- use original header list for Tabledownload, since we modified it before
                                (TableDownload <| TableDataTagged tableDataTagged.tag headers_ tableDataTagged.rows tableDataTagged.dataFormats)
                                tableDataTagged
                        )
        in
        div []
            (h3
                [ class "uk-heading-line uk-text-center" ]
                [ span
                    [ class "uk-text-background uk-text-large" ]
                    [ text taggedRecordsText ]
                ]
                :: rowsViews
            )



-- HELPERS


matchRows : Dict String String -> List String -> List Row -> List Row
matchRows batchTaggingOptions headers rows =
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
                Dict.size batchTaggingOptions > 0 && Dict.size batchTaggingOptions == matchingCellCount
            )
        |> List.map mapRowCellsToRemoveColumns


unwrapRows : List Table.Row -> List (List String)
unwrapRows records =
    List.map .cells records


getRegexDropdown key =
    Dict.fromList
        [ ( "Integers", [ ( "Integer", UserClickedItemInPlaceRegexDropDownList key IntegerRegex ) ] )
        , ( "Period Floats", [ ( "Period Float", UserClickedItemInPlaceRegexDropDownList key PeriodFloatRegex ) ] )
        , ( "Comma Floats", [ ( "Comma Float", UserClickedItemInPlaceRegexDropDownList key CommaFloatRegex ) ] )
        ]


getWorkingTaggedData : List (List Table.TableDataTagged) -> Maybe (List Table.TableDataTagged)
getWorkingTaggedData tableDataTagged =
    List.head tableDataTagged


getFirstTaggedDataItem : Model -> Maybe Table.TableDataTagged
getFirstTaggedDataItem model =
    getWorkingTaggedData model.tableDataTagged |> Maybe.andThen List.head


getWorkingData : Model -> Maybe Table.TableData
getWorkingData model =
    List.head model.tableData


getWorkspace : Model -> Workspace
getWorkspace model =
    let
        workingTableDataHeaders =
            getWorkingData model |> Maybe.map .headers

        taggedTableDataHeaders =
            getFirstTaggedDataItem model |> Maybe.map .headers
    in
    case ( workingTableDataHeaders, taggedTableDataHeaders ) of
        ( Just headersA, Just _ ) ->
            Workspace headersA

        ( Just headersA, Nothing ) ->
            Workspace headersA

        ( Nothing, Just headersB ) ->
            Workspace <| List.drop 1 headersB

        ( Nothing, Nothing ) ->
            EmptyWorkspace


partitionIrregularRowsInLength : List String -> List (List String) -> ( List (List String), List (List String) )
partitionIrregularRowsInLength headers records =
    let
        validLength =
            List.length headers
    in
    List.partition
        (\record ->
            List.length record > validLength
        )
        records


mapRowCellsToHaveColumns : List ColumnHeadingName -> Row -> { cells : List ( ColumnHeadingName, Cell ) }
mapRowCellsToHaveColumns headers row =
    { cells = List.map2 Tuple.pair headers row.cells }


mapRowCellsToRemoveColumns : { cells : List ( ColumnHeadingName, Cell ) } -> Row
mapRowCellsToRemoveColumns row =
    let
        newCells =
            List.map (\( _, cell ) -> cell) row.cells
    in
    Row newCells


setTagInstance : Tag -> List ColumnHeadingName -> List TableDataTagged -> List TableDataTagged
setTagInstance theTag headers tableDataTagged =
    let
        doesInstanceExist =
            List.any (\model -> model.tag == theTag) tableDataTagged
    in
    if not doesInstanceExist then
        { headers = headers, tag = theTag, rows = [], dataFormats = Dict.empty } :: tableDataTagged

    else
        tableDataTagged


mapRowToTag : String -> Row -> TableDataTagged -> TableDataTagged
mapRowToTag aTag aRow aTaggedTable =
    if aTaggedTable.tag == aTag then
        let
            { headers, tag, rows, dataFormats } =
                aTaggedTable
        in
        TableDataTagged tag headers (aRow :: rows) dataFormats |> Table.detectDataFormats

    else
        aTaggedTable


parseCsvString : Char -> String -> Csv.Csv
parseCsvString separator contents =
    contents
        |> Csv.parseWith separator


createTableDataFromCsv : ImportStacking -> Csv.Csv -> Model -> Model
createTableDataFromCsv stacking csv model =
    let
        recordsConvertedToRows =
            List.map (\row -> List.map String.trim row) csv.records
                |> List.map Row
    in
    case stacking of
        Stack ->
            let
                currentTableData =
                    Maybe.withDefault (TableData [] []) (List.head model.tableData)

                updatedTableData =
                    TableData csv.headers <| currentTableData.rows ++ recordsConvertedToRows

                updatedSelectedWorkingData =
                    if Dict.isEmpty model.batchTaggingOptions then
                        updatedTableData

                    else
                        matchRows model.batchTaggingOptions updatedTableData.headers updatedTableData.rows
                            |> TableData updatedTableData.headers
            in
            { model | tableData = [ updatedTableData ], selectedWorkingData = updatedSelectedWorkingData }

        Replace ->
            let
                newTableData =
                    TableData csv.headers recordsConvertedToRows
            in
            { model | tableData = [ newTableData ], selectedWorkingData = newTableData }


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
