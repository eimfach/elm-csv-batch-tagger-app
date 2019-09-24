module Data.Table exposing (Cell, Currency(..), DataFormat(..), Row, TableData, TableDataTagged, decodeTableDataList, decodeTableDataTaggedList, detectDataFormats, encodeTableData, encodeTableDataTagged, flattenRows, getColumnData, getColumnDataWith, getColumnDataWithParser, parseCurrencyToFloat, prependCellToRow, setADataFormat)

import Data.Alias exposing (ColumnHeadingName, Tag)
import Data.Helpers exposing (isResultOk)
import Data.Parsers exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Parser exposing ((|.), (|=))


type alias Cell =
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



{- @Data.Table.TableDataTagged deprecated, move tags into Row type -}


type alias TableDataTagged =
    { tag : Tag
    , headers : List ColumnHeadingName
    , rows : List Row
    , dataFormats : Dict ColumnHeadingName DataFormat
    }



-- "↓" "↑"
-- Type Sort
-- if every column of a dataset is unordered, the whole dataset is unordered
-- if one ore multiple columns are ordererd (asc or desc) by default the dataset is ordered without any further doings.
-- It doesn't matter how much are ordered, the whole dataset goes as sorted but we can't say sorted by which column.
-- as a result we need to know each columns sorting state and reflect that in the types signature


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
        |. Data.Parsers.parseOptionalSpaces
        |. parseCurrencySymbol currencySymbol
        |. Parser.end
        |> Parser.andThen Data.Parsers.convertToFloat


parseCurrency : Currency -> Parser.Parser Currency
parseCurrency selectedCurrency =
    let
        currencySymbol =
            convertCurrencyToSymbol selectedCurrency
    in
    Parser.succeed identity
        |. parseChainFloat
        |. Data.Parsers.parseOptionalSpaces
        |= parseCurrencySymbol currencySymbol
        |. Parser.end
        |> Parser.andThen convertToCurrency


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


flattenRows : List Row -> List (List String)
flattenRows someRows =
    List.map (\row -> row.cells) someRows


prependCellToRow : String -> Row -> Row
prependCellToRow cell aRow =
    { aRow | cells = cell :: aRow.cells }


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
