module Main exposing (main)

import Base64
import Csv exposing (Csv)
import Data.Alias exposing (ColumnHeadingName, HtmlNodeId, Keyword, Tag)
import Data.Button
import Data.Modal
import Data.Structure as Structure
import Data.Table exposing (Row, TableData, TableDataTagged, flattenRows)
import Html exposing (Html, a, button, datalist, dd, div, dl, dt, h1, h2, h3, h4, h5, hr, input, label, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, href, id, name, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import List.Extra as ListExtra
import Ports.FileReader exposing (FileData, fileContentRead, fileSelected)
import Regex exposing (Regex)
import Section.ApplyTags exposing (viewAutoTaggingTab, viewManualTaggingTab)
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


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
    , keyWordInputBuffer : Keyword
    , mapTagInputBuffer : String
    , fileUploadPointerId : HtmlNodeId
    , file : Maybe FileData
    , tableData : List TableData
    , tableDataTagged : List (List TableDataTagged)
    , autoTagPointer : ColumnHeadingName
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
    | SetAutoTagPointer ColumnHeadingName
    | KeyWordInput Keyword
    | SetTaggingOption TaggingOption
    | NoOp
    | OpenModal Data.Modal.Title HtmlNode Msg Msg
    | OpenModalInfo Data.Modal.Title HtmlNode Msg
    | CloseModal
    | UndoMapRecordToTag UndoStrategy


init : ( Model, Cmd Msg )
init =
    let
        tags =
            [ "haushalt", "hobby", "horem", "ipsum" ]
                |> Set.fromList
    in
    ( { tags = tags
      , addTagInputBuffer = ""
      , addTagInputError = ( "", False )
      , keyWordInputBuffer = ""
      , mapTagInputBuffer = ""
      , fileUploadPointerId = "csv-upload"
      , file = Nothing
      , tableData = []
      , tableDataTagged = []
      , autoTagPointer = ""
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

        SetAutoTagPointer column ->
            ( { model | autoTagPointer = column }, Cmd.none )

        KeyWordInput val ->
            ( { model | keyWordInputBuffer = val }, Cmd.none )

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
                [ viewTaggingSection model.optionTagging model.autoTagPointer model.keyWordInputBuffer model.tags tableData.headers currentRow tableData.rows taggingSectionNav
                ]
            ]
        , div
            [ class "row" ]
            [ div [ class "col-lg-12 col-sm-12" ]
                [ viewMappedRecordsPanel tableData.headers tableDataTagged
                ]
            ]
        ]


viewTaggingSection : TaggingOption -> HtmlNodeId -> Keyword -> Set Tag -> List ColumnHeadingName -> Row -> List Row -> HtmlNode -> HtmlNode
viewTaggingSection taggingOption autoTagPointer keyword tags headers row rows nav =
    let
        colIndex =
            getColIndex 0 autoTagPointer headers

        taggingAction tag =
            case taggingOption of
                SingleTagging ->
                    MapRecordToTag (Structure.Single row) tag

                BatchTagging ->
                    let
                        pattern =
                            Regex.caseInsensitive (Regex.regex keyword)

                        autoMatchedRecords =
                            if String.isEmpty keyword then
                                []

                            else
                                {--extract records by keyword --}
                                rows
                                    |> List.filter
                                        (\row ->
                                            ListExtra.getAt colIndex row.cells
                                                |> Maybe.withDefault ""
                                                |> Regex.contains pattern
                                        )

                        plainMatchedRecords =
                            rowPlain autoMatchedRecords

                        openModal records =
                            if List.isEmpty records then
                                OpenModalInfo
                                    "Records that will be tagged"
                                    (text "There were no matching records found")
                                    CloseModal

                            else
                                OpenModal
                                    "Records that will be tagged"
                                    (Table.view headers records)
                                    (MapRecordToTag (Structure.Multiple autoMatchedRecords) tag)
                                    CloseModal
                    in
                    openModal plainMatchedRecords

        ( singleIsActiveTab, viewTab ) =
            case taggingOption of
                {--tab selection is naive--}
                SingleTagging ->
                    ( True, viewManualTaggingTab headers row.cells )

                BatchTagging ->
                    ( False, viewAutoTaggingTab colIndex KeyWordInput SetAutoTagPointer headers rows )
    in
    if List.isEmpty row.cells then
        text ""

    else
        div []
            [ div [ class "uk-position-relative" ]
                [ h3
                    [ class "uk-heading-line uk-text-center" ]
                    [ span [ class "uk-text-background uk-text-bold uk-text-large" ]
                        [ text ("Apply tags (" ++ toString (List.length rows) ++ " left)")
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
            , p
                []
                [ Autocomplete.view
                    "tag-autocomplete"
                    [ onInput MapTagInput, placeholder "Search for tag" ]
                    tags
                ]
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


getColIndex : Int -> String -> List String -> Int
getColIndex default colName columns =
    case ListExtra.elemIndex colName columns of
        Just index ->
            index

        Nothing ->
            default


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
