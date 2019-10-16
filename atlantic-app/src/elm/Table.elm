module Table exposing (Cell, ColumnHeadingName, Currency(..), DataFormat(..), Row, TableData, TableDataTagged, Tag, decodeTableDataList, decodeTableDataTaggedList, detectDataFormats, encodeTableData, encodeTableDataTagged, flattenRows, getColumnData, getColumnDataWith, getColumnDataWithParser, parseCurrencyToFloat, prependCellToRow, setADataFormat, view, viewSingle, viewWithTagData)

import Dict exposing (Dict)
import Helpers exposing (isResultOk)
import Html exposing (Html, div, h3, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import NavBar
import Parser exposing ((|.), (|=))
import Parsers exposing (..)


type alias Cell =
    String


type alias ColumnHeadingName =
    String


type alias Tag =
    String


type alias TableData =
    { headers : List ColumnHeadingName
    , rows : List Row
    }


type alias Row =
    { cells : List Cell
    }


type DataFormat
    = Text
    | Float
    | Integer
    | Date
    | Currency Currency


type Currency
    = Dollar
    | Euro


type alias TableDataTagged =
    { tag : Tag
    , headers : List ColumnHeadingName
    , rows : List Row
    , dataFormats : Dict ColumnHeadingName DataFormat
    }



-- PARSING


convertCurrencyToSymbol : Currency -> String
convertCurrencyToSymbol selectedCurrency =
    case selectedCurrency of
        Dollar ->
            "$"

        Euro ->
            "€"


parseCurrencySymbol : String -> Parser.Parser String
parseCurrencySymbol currencySymbol =
    Parser.map (always currencySymbol) (Parser.symbol currencySymbol)


parseCurrencyToFloat : Currency -> Parser.Parser Float
parseCurrencyToFloat selectedCurrency =
    let
        currencySymbol =
            convertCurrencyToSymbol selectedCurrency
    in
    Parser.succeed identity
        |= parseChainFloat
        |. Parsers.parseOptionalSpaces
        |. parseCurrencySymbol currencySymbol
        |. Parser.end
        |> Parser.andThen Parsers.convertToFloat


parseCurrency : Currency -> Parser.Parser Currency
parseCurrency selectedCurrency =
    let
        currencySymbol =
            convertCurrencyToSymbol selectedCurrency
    in
    Parser.succeed identity
        |. parseChainFloat
        |. Parsers.parseOptionalSpaces
        |= parseCurrencySymbol currencySymbol
        |. Parser.end
        |> Parser.andThen convertToCurrency



-- ENCODING


convertToCurrency : String -> Parser.Parser Currency
convertToCurrency currency =
    case currency of
        "€" ->
            Parser.succeed <| Euro

        "$" ->
            Parser.succeed <| Dollar

        _ ->
            Parser.problem "currency token expected"


encodeTableDataTagged : TableDataTagged -> Encode.Value
encodeTableDataTagged tableData =
    Encode.object
        [ ( "tag", Encode.string tableData.tag )
        , ( "headers", Encode.list Encode.string tableData.headers )
        , ( "rows", Encode.list encodeRow tableData.rows )
        , ( "dataFormats", Encode.dict identity encodeDataFormat tableData.dataFormats )
        ]


encodeTableData : TableData -> Encode.Value
encodeTableData tableData =
    Encode.object
        [ ( "headers", Encode.list Encode.string tableData.headers )
        , ( "rows", Encode.list encodeRow tableData.rows )
        ]


encodeRow : Row -> Encode.Value
encodeRow row =
    Encode.object
        [ ( "cells", Encode.list Encode.string row.cells )
        ]



-- DECODING


decodeTableData : Decode.Decoder TableData
decodeTableData =
    Decode.map2 TableData
        (Decode.field "headers" (Decode.list Decode.string))
        (Decode.field "rows" (Decode.list (Decode.map Row (Decode.field "cells" (Decode.list Decode.string)))))


decodeTableDataTagged : Decode.Decoder TableDataTagged
decodeTableDataTagged =
    Decode.map4 TableDataTagged
        (Decode.field "tag" Decode.string)
        (Decode.field "headers" (Decode.list Decode.string))
        (Decode.field "rows" (Decode.list (Decode.map Row (Decode.field "cells" (Decode.list Decode.string)))))
        (Decode.field "dataFormats" (Decode.dict dataFormatDecoder))


decodeTableDataTaggedWithoutDataFormats : Decode.Decoder TableDataTagged
decodeTableDataTaggedWithoutDataFormats =
    Decode.map3 (\tag headers rows -> detectDataFormats <| TableDataTagged tag headers rows Dict.empty)
        (Decode.field "tag" Decode.string)
        (Decode.field "headers" (Decode.list Decode.string))
        (Decode.field "rows" (Decode.list (Decode.map Row (Decode.field "cells" (Decode.list Decode.string)))))


decodeTableDataList : Decode.Value -> String -> Result String (List TableData)
decodeTableDataList decodedValue fieldName =
    let
        tableData =
            Decode.decodeValue (Decode.field fieldName (Decode.list decodeTableData)) decodedValue
    in
    case tableData of
        Ok tableData_ ->
            Ok tableData_

        Err err_ ->
            Err ("Error parsing " ++ fieldName ++ "." ++ "tableData: " ++ Decode.errorToString err_)


decodeTableDataTaggedList : Decode.Value -> String -> Result String (List (List TableDataTagged))
decodeTableDataTaggedList decodedValue fieldName =
    let
        tableDataTagged =
            Decode.decodeValue (Decode.field fieldName (Decode.list (Decode.list <| Decode.oneOf [ decodeTableDataTagged, decodeTableDataTaggedWithoutDataFormats ]))) decodedValue
    in
    case tableDataTagged of
        Ok tableDataTagged_ ->
            Ok tableDataTagged_

        Err err_ ->
            Err ("Error parsing " ++ fieldName ++ "." ++ "tableDataTagged: " ++ Decode.errorToString err_)


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



-- DATAFORMATS


setADataFormat : ColumnHeadingName -> DataFormat -> TableDataTagged -> TableDataTagged
setADataFormat column dataFormat taggedTable =
    { taggedTable | dataFormats = Dict.insert column dataFormat taggedTable.dataFormats }


detectDataFormats : TableDataTagged -> TableDataTagged
detectDataFormats taggedTable =
    List.indexedMap
        (\colIndex column ->
            case getColumnDataWithParser parseFloat colIndex taggedTable.rows of
                Just _ ->
                    setADataFormat column Float

                Nothing ->
                    case getColumnDataWithParser parseInt colIndex taggedTable.rows of
                        Just _ ->
                            setADataFormat column Integer

                        Nothing ->
                            case getColumnDataWithParser parseAnySupportedDate colIndex taggedTable.rows of
                                Just _ ->
                                    setADataFormat column Date

                                Nothing ->
                                    case getColumnDataWithParser (parseCurrency Dollar) colIndex taggedTable.rows of
                                        Just _ ->
                                            setADataFormat column (Currency Dollar)

                                        Nothing ->
                                            case getColumnDataWithParser (parseCurrency Euro) colIndex taggedTable.rows of
                                                Just _ ->
                                                    setADataFormat column (Currency Euro)

                                                Nothing ->
                                                    setADataFormat column Text
        )
        taggedTable.headers
        |> List.foldl (\updatePart newTableData -> updatePart newTableData) taggedTable



-- MANIPULATE ROWS


flattenRows : List Row -> List (List String)
flattenRows someRows =
    List.map (\row -> row.cells) someRows


prependCellToRow : String -> Row -> Row
prependCellToRow cell aRow =
    { aRow | cells = cell :: aRow.cells }



-- GET COLUMNS


getColumnData : Int -> List Row -> List String
getColumnData columnIndex records =
    List.foldl (.cells >> List.Extra.getAt columnIndex >> Maybe.withDefault "" >> List.singleton >> List.append) [] records


getColumnDataWithParser : Parser.Parser a -> Int -> List Row -> Maybe (List a)
getColumnDataWithParser parser columnIndex records =
    let
        columnData =
            getColumnData columnIndex records
                |> List.map (Parser.run parser)
    in
    if List.all isResultOk columnData then
        Just (List.map Result.toMaybe columnData |> List.filterMap identity)

    else
        Nothing


getColumnDataWith : (String -> Maybe a) -> Int -> List Row -> Maybe (List a)
getColumnDataWith parser columnIndex records =
    let
        columnData =
            getColumnData columnIndex records
                |> List.map parser
    in
    if List.any ((==) Nothing) columnData then
        Nothing

    else
        Just (List.filterMap identity columnData)



-- VIEW


viewSingle : List (Html.Attribute msg) -> List String -> List (Html msg) -> Html msg
viewSingle cellAttr headers record =
    div [ class "uk-overflow-auto" ]
        [ table
            [ class "uk-table uk-table-responsive uk-table-divider" ]
            [ thead []
                [ viewRow th [] <| List.map text headers ]
            , tbody []
                [ viewRow td cellAttr record ]
            ]
        ]


view : List ( String, msg ) -> List (List (Html msg)) -> Html msg
view headers rows =
    table [ class "uk-table uk-table-responsive uk-table-divider uk-table-middle uk-table-small" ]
        [ thead []
            [ viewRow th [] <| List.map (\( column, msg ) -> span [ onClick msg ] [ text column ]) headers ]
        , tbody []
            (List.map (\row -> viewRow td [] row) rows)
        ]


viewRow : (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg) -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewRow tableElement elementAttr cells =
    tr []
        (List.map
            (\content -> tableElement elementAttr [ content ])
            cells
        )


viewWithTagData : msg -> { tag : Tag, headers : List ( ColumnHeadingName, msg ), rows : List Row, dataFormats : Dict ColumnHeadingName DataFormat } -> Html msg
viewWithTagData exportAction { tag, headers, rows } =
    let
        plainPreparedRows =
            rows |> flattenRows |> List.map (List.map text)
    in
    div [ class "uk-padding-small uk-overflow-auto" ]
        [ p
            [ class "uk-position-relative" ]
            [ h3
                [ class "uk-text-center" ]
                [ span
                    [ class "uk-text-large" ]
                    [ text tag ]
                ]
            , NavBar.viewIconNav [ ( NavBar.Export, exportAction, [] ) ]
            ]
        , view
            headers
            plainPreparedRows
        ]
