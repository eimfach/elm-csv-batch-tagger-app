module Data.Table exposing (Cell, Row, TableData, TableDataTagged, decodeTableDataList, decodeTableDataTaggedList, encodeRow, encodeTableData, encodeTableDataTagged, flattenRows, getColumnData, getColumnDataWithParser, prependCellToRow)

import Data.Alias exposing (ColumnHeadingName, Tag)
import Data.Helpers exposing (isResultOk, maybeToBool)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Parser


type alias Cell =
    String


type alias TableData =
    { headers : List ColumnHeadingName
    , rows : List Row
    }


type alias Row =
    { cells : List Cell
    }



{- @Data.Table.TableDataTagged deprecated, move tags into Row type -}


type alias TableDataTagged =
    { tag : Tag
    , headers : List ColumnHeadingName
    , rows : List Row
    }



-- "↓" "↑"
-- Type Sort
-- if every column of a dataset is unordered, the whole dataset is unordered
-- if one ore multiple columns are ordererd (asc or desc) by default the dataset is ordered without any further doings.
-- It doesn't matter how much are ordered, the whole dataset goes as sorted but we can't say sorted by which column.
-- as a result we need to know each columns sorting state and reflect that in the types signature


encodeTableDataTagged : TableDataTagged -> Encode.Value
encodeTableDataTagged tableData =
    Encode.object
        [ ( "tag", Encode.string tableData.tag )
        , ( "headers", Encode.list Encode.string tableData.headers )
        , ( "rows", Encode.list encodeRow tableData.rows )
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
    Decode.map3 TableDataTagged
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
            Decode.decodeValue (Decode.field fieldName (Decode.list (Decode.list decodeTableDataTagged))) decodedValue
    in
    case tableDataTagged of
        Ok tableDataTagged_ ->
            Ok tableDataTagged_

        Err err_ ->
            Err ("Error parsing " ++ fieldName ++ "." ++ "tableDataTagged: " ++ Decode.errorToString err_)


flattenRows : List Row -> List (List String)
flattenRows someRows =
    List.map (\row -> row.cells) someRows


prependCellToRow : String -> Row -> Row
prependCellToRow cell aRow =
    { aRow | cells = cell :: aRow.cells }


getColumnData : Int -> List Row -> List String
getColumnData columnIndex records =
    List.foldl (.cells >> List.Extra.getAt columnIndex >> Maybe.withDefault "" >> List.singleton >> List.append) [] records


getColumnDataWithParser : Parser.Parser Float -> Int -> List Row -> Maybe (List Float)
getColumnDataWithParser parseFloat columnIndex records =
    let
        columnData =
            getColumnData columnIndex records
                |> List.map (Parser.run parseFloat)
    in
    if List.all isResultOk columnData then
        Just (List.map Result.toMaybe columnData |> List.filterMap identity)

    else
        Nothing
