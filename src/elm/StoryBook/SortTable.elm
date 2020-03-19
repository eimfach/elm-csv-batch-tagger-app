module StoryBook.SortTable exposing (..)

import Browser
import Dict
import Html exposing (Html)
import List.Extra
import Table


type alias Model =
    Table.TableDataTagged


records : List (List String)
records =
    [ [ "1 $"
      , "01.01.2019"
      , "2012-05-02"
      , "3214"
      , "3422,43"
      , "2020-08-01"
      ]
    , [ "32.023 $"
      , "01.02.2019"
      , "2014-10-25"
      , "1"
      , "432.0234"
      , "2024-07-15"
      ]
    , [ "23,12 $"
      , "22.03.2013"
      , "2011-11-09"
      , "321"
      , "-342,0342"
      , "21.03.2017"
      ]
    , [ "18.431 $"
      , "04.06.2015"
      , "2020-12-22"
      , "44"
      , "-34.432"
      , "01.01.2000"
      ]
    , [ "-123 $"
      , "05.03.2025"
      , "2002-09-08"
      , "-23"
      , "423,432"
      , "1989-09-01"
      ]
    , [ "31 $"
      , "05.09.2025"
      , "2002-06-12"
      , "432"
      , "654.33"
      , "01.01.1970"
      ]
    ]


headers =
    [ "Lorem", "Ipsum", "Solor", "Dit", "Amed", "Consectetur" ]


init =
    Table.TableDataTagged "Finance" headers (List.map Table.Row records) Dict.empty
        |> Table.detectDataFormats


type Msg
    = Sort String
    | NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update msg model =
    case msg of
        Sort column ->
            case List.Extra.elemIndex column model.headers of
                Just columnIndex ->
                    let
                        sortedTable =
                            Table.sort column columnIndex model
                    in
                    ( sortedTable, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        headersWithActions =
            List.map (\column -> ( column, Sort column )) model.headers
    in
    Table.viewWithTagData Table.Unresponsive NoOp { tag = model.tag, headers = headersWithActions, rows = model.rows, dataFormats = model.dataFormats }
