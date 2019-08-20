module Data.Table exposing (Cell, Row, TableData, TableDataTagged, flattenRows, prependCellToRow)

import Data.Alias exposing (ColumnHeadingName, Tag)
import Unique exposing (Unique)


type alias Cell =
    String


type alias TableData =
    { headers : List ColumnHeadingName
    , rows : List Row
    }


type alias Row =
    { id : Unique.Id
    , cells : List Cell
    , tags : List Tag
    }



{- @Data.Table.TableDataTagged deprecated, move tags into Row type -}


type alias TableDataTagged =
    { tag : Tag
    , headers : List ColumnHeadingName
    , rows : List Row
    }


flattenRows : List Row -> List (List String)
flattenRows someRows =
    List.map (\row -> row.cells) someRows


prependCellToRow : String -> Row -> Row
prependCellToRow cell aRow =
    { aRow | cells = cell :: aRow.cells }
