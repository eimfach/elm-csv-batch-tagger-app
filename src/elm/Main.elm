port module Main exposing (main)

import Browser
import Browser.Navigation
import Button
import Csv
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import File.Select
import Html exposing (Html, a, button, div, h3, h5, hr, input, label, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, href, id, name, placeholder, style, target, type_, value)
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
import Table as Table exposing (Cell, ColumnHeadingName, Row, TableData, TableDataTagged, Tag, decodeTableDataList, decodeTableDataTaggedList, encodeTableData, encodeTableDataTagged, flattenRows, parseCurrencyToFloat, prependCellToRow)
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
    = ViewImportFileRecords (List ColumnHeadingName) (List (List String))
    | ViewMapRecordsToTag (List ColumnHeadingName) (List (List String)) Tag
    | ViewInfo String
    | ViewWarningDeleteLocalData


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
    , optionTagging : TaggingOption
    , showModal : Maybe (Modal.State ModalContent)
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
    in
    ( { locale = locale
      , tags = tags
      , addTagInputBuffer = addTagInputBuffer
      , addTagInputError = addTagInputError
      , fileUploadPointerId = fileUploadPointerId
      , tableData = tableData
      , tableDataTagged = tableDataTagged
      , batchTaggingOptions = batchTaggingOptions
      , optionTagging = BatchTagging
      , showModal = showModal
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
        ]


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
        [ Decode.field "viewImportFileRecords"
            (Decode.map2 ViewImportFileRecords
                (Decode.field "columns" <| Decode.list Decode.string)
                (Decode.field "records" <| Decode.list (Decode.list Decode.string))
            )
        , Decode.field "viewMapRecordsToTag"
            (Decode.map3 ViewMapRecordsToTag
                (Decode.field "columns" <| Decode.list Decode.string)
                (Decode.field "records" <| Decode.list (Decode.list Decode.string))
                (Decode.field "tag" Decode.string)
            )
        , Decode.field "viewInfo" (Decode.string |> Decode.map ViewInfo)
        , Decode.field "viewWarningDeleteLocalData" (Decode.succeed ViewWarningDeleteLocalData)
        ]


encodeModalContent : ModalContent -> Encode.Value
encodeModalContent modalContent_ =
    case modalContent_ of
        ViewImportFileRecords columns records ->
            let
                valuesEncoding =
                    Encode.object
                        [ ( "columns", Encode.list Encode.string columns )
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



-- UPDATE


{-| SomeoneDidSomethingSomewhereAndSomeHow
-}
type Msg
    = RemoveTag String
    | UserClickedRequestDeleteDataButton
    | DeleteLocalData
    | ToggleLocale
    | SetLocale String
    | TagInput String
    | CreateTagFromBuffer
    | MapRecordToTag (Bucket Row) Tag
    | SearchPatternInput ColumnHeadingName SearchPattern
    | SetTaggingOption TaggingOption
    | NoOp
    | CloseModal
    | UndoMapRecordToTag UndoStrategy
    | TableDownload TableDataTagged
    | ShowMatchingRecords (List ColumnHeadingName) Tag (List Row)
    | SortTaggedTable Tag ColumnHeadingName
    | UserClickedFileSelectButton
    | UserSelectedFileFromSysDialog File
    | CmdCompletedFileLoadingTask String


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
        UserClickedFileSelectButton ->
            ( model, File.Select.file [ "text/csv" ] UserSelectedFileFromSysDialog )

        UserSelectedFileFromSysDialog file ->
            ( model, Task.perform CmdCompletedFileLoadingTask (File.toString file) )

        CmdCompletedFileLoadingTask content ->
            let
                showImportConfirmation csv model_ =
                    updateShowModal
                        Modal.RegularView
                        (translateImportData model_.locale)
                        (ViewImportFileRecords csv.headers csv.records)
                        model_

                newModel =
                    case parseCsvString ';' content of
                        Ok csv ->
                            showImportConfirmation csv model

                        Err _ ->
                            case parseCsvString ',' content of
                                Ok csv ->
                                    -- @TODO: show dialog first
                                    showImportConfirmation csv model

                                -- createTableDataFromCsv csv model
                                Err _ ->
                                    updateShowModalInfo
                                        (Locale.translateErrorHeading model.locale)
                                        (ViewInfo <| Locale.translateErrorParsingYourFile model.locale)
                                        model
            in
            ( newModel |> updateResetBatchTaggingOptions
            , Cmd.none
            )

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

        ToggleLocale ->
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

        ShowMatchingRecords headers tag rows ->
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
                    Table.rowPlain matchedRowsAsRowType

                modalTitleText =
                    Locale.translateRecordsThatWillBeTagged model.locale (List.length plainMatchedRecords)

                modalDisplay =
                    if List.isEmpty plainMatchedRecords then
                        Modal.RegularView

                    else
                        Modal.Fullscreen
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


updateShowModalInfo : Modal.Title -> ModalContent -> Model -> Model
updateShowModalInfo title content model =
    { model | showModal = Just (Modal.State content title Modal.RegularView) }


updateShowModal : Modal.DisplayProperties -> Modal.Title -> ModalContent -> Model -> Model
updateShowModal displayProperties title content model =
    { model | showModal = Just (Modal.State content title displayProperties) }


updateCloseModal : Model -> Model
updateCloseModal model =
    { model | showModal = Nothing }


updateResetBatchTaggingOptions : Model -> Model
updateResetBatchTaggingOptions model =
    { model | batchTaggingOptions = Dict.empty }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ getLocale SetLocale
        ]



-- VIEW


viewRecords : List ColumnHeadingName -> List (List String) -> Html.Html Msg
viewRecords headers records =
    Table.view
        Table.Responsive
        (List.map (\column -> ( column, NoOp )) headers)
        (List.map (List.map text) records)


viewModalContent : Locale -> ModalContent -> Html.Html Msg
viewModalContent locale modalContent =
    case modalContent of
        ViewImportFileRecords headers records ->
            if List.isEmpty records then
                text <| Locale.translateImportDataNoRecordsFound locale

            else
                viewRecords headers records

        ViewMapRecordsToTag headers plainRecords _ ->
            if List.isEmpty plainRecords then
                text <| Locale.translateNoMatchingRecordsFound locale

            else
                viewRecords headers plainRecords

        ViewInfo info ->
            text info

        ViewWarningDeleteLocalData ->
            text (Locale.translateWarningDeleteLocalData locale)


getModalButtons : Locale -> ModalContent -> List (Modal.Button Msg)
getModalButtons locale modalContent =
    case modalContent of
        ViewImportFileRecords _ records ->
            if List.isEmpty records then
                [ Modal.DefaultButton Button.Secondary CloseModal (Locale.translateCancel locale)
                ]

            else
                [ Modal.IconButton Button.Primary CloseModal (Locale.translateImport locale) Button.Import
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
            [ Modal.DefaultButton Button.Secondary DeleteLocalData (Locale.translateProceed locale)
            , Modal.DefaultButton Button.Primary CloseModal (Locale.translateCancel locale)
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
                , a [ href "https://www.robingruenke.com/journal/documentation/tools/documentation-for-my-csv-batch-tagging-tool.html", class "uk-icon-book uk-text-success", target "_blank" ] [ text (Locale.translateViewUserDocumentation model.locale) ]
                ]
            , div [ class "uk-margin-top" ]
                [ button [ class "uk-button uk-align-right", onClick ToggleLocale ]
                    [ text <| Locale.translateLocale model.locale ++ ": " ++ localeTranslation ]
                , span
                    [ class "uk-label uk-text-small" ]
                    [ text "NOTE" ]
                , span [ class "uk-text-small uk-text-light" ] [ text <| "   " ++ Locale.translateInfoOnHowDataIsStored model.locale ]
                , button [ class "uk-button-link uk-margin-left", style "border" "none", onClick UserClickedRequestDeleteDataButton ] [ text (Locale.translateDeleteYourLocalData model.locale) ]
                ]
            , div []
                [ viewFileUploadSection (Locale.translateSelectAcsvFile model.locale) ]
            , div []
                [ viewManageTagsSection (Locale.translateManageYourTags model.locale) (Locale.translateEnterATag model.locale) model.addTagInputError model.addTagInputBuffer model.tags TagInput CreateTagFromBuffer RemoveTag
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
                [ viewMappedRecordsPanel (Locale.translateTag model.locale) (Locale.translateTaggedRecords model.locale) tableData.headers tableDataTagged
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


viewTagCloud : (Table.Tag -> msg) -> Set Table.Tag -> Html msg
viewTagCloud action tags =
    p
        [ class "tag-cloud" ]
        (viewTagButtons
            action
            tags
        )



-- VIEW SECTIONS


viewFileUploadSection : String -> Html Msg
viewFileUploadSection headerText =
    div [ class "uk-section uk-section-small" ]
        [ h3
            [ class "uk-heading-line uk-text-center" ]
            [ span [ class "uk-text-background uk-text-large" ] [ text headerText ]
            ]
        , div
            [ class "uk-padding" ]
            [ button
                [ onClick UserClickedFileSelectButton
                , class "file-upload-button uk-button uk-button-default uk-width-1-1 uk-margin"
                ]
                [ label
                    [ attribute "uk-icon" "icon: upload"
                    , for "csv-upload"
                    , class "file-label"
                    ]
                    [ text "" ]
                ]
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


viewTaggingSection : Locale -> TaggingOption -> Dict ColumnHeadingName SearchPattern -> Set Tag -> List ColumnHeadingName -> Row -> List Row -> HtmlNode -> HtmlNode
viewTaggingSection locale taggingOption batchTaggingOptions tags headers row rows nav =
    let
        translateHeaderText =
            Locale.translateApplyTags locale

        singleTaggingText =
            Locale.translateSingleTagging locale

        batchTaggingText =
            Locale.translateBatchTagging locale

        tagActionText =
            Locale.translateSelectATagToTag locale

        taggingAction tag =
            case taggingOption of
                SingleTagging ->
                    MapRecordToTag (Single row) tag

                BatchTagging ->
                    ShowMatchingRecords headers tag rows

        ( viewTagActionDescription, viewTagCloud_ ) =
            if List.isEmpty rows then
                ( text "", text "" )

            else
                ( div [ class "uk-margin" ] [ h5 [ class "uk-text-primary" ] [ text tagActionText ] ]
                , viewTagCloud (\tag -> taggingAction tag) tags
                )

        ( singleIsActiveTab, viewTab ) =
            case taggingOption of
                {--tab selection is naive--}
                SingleTagging ->
                    ( True, viewManualTaggingTab locale headers row.cells )

                BatchTagging ->
                    ( False, viewBatchTaggingTab locale batchTaggingOptions SearchPatternInput headers rows )
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
            , viewTagActionDescription
            , viewTagCloud_
            , hr [ class "uk-divider-icon" ] []
            ]
        ]


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
                , Table.viewSingle
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


viewBatchTaggingInput : String -> Locale.Locale -> String -> String -> Set String -> (SearchPattern -> msg) -> Html.Html msg
viewBatchTaggingInput labelText locale idVal val options action =
    Input.viewAutocomplete
        labelText
        val
        idVal
        [ placeholder (Locale.translateSelectAKeywordOrRegex locale), onInput action ]
        options


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


parseCsvString : Char -> String -> Result (List Parser.DeadEnd) Csv.Csv
parseCsvString separator contents =
    contents
        |> Csv.parseWith separator


createTableDataFromCsv : Csv.Csv -> Model -> Model
createTableDataFromCsv csv model =
    let
        recordsConvertedToRows =
            List.map (\row -> List.map String.trim row) csv.records
                |> List.map Row
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
