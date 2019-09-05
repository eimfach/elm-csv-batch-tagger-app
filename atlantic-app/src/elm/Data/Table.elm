module Data.Table exposing (Cell, Row, TableData, TableDataTagged, decodeTableDataList, decodeTableDataTaggedList, encodeRow, encodeTableData, encodeTableDataTagged, flattenRows, prependCellToRow)

import Data.Alias exposing (ColumnHeadingName, Tag)
import Json.Decode as Decode
import Json.Encode as Encode
import Unique exposing (Unique)


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
