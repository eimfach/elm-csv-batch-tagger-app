module Parsers exposing (convertToFloat, parseAnySupportedDate, parseChainFloat, parseEuropeanDateToISO8601String, parseEuropeanDateToPosix, parseFloat, parseInt, parseIso8601Date, parseIso8601DateToPosix, parseOptionalSpaces)

import Parser exposing ((|.), (|=))
import Time
import Time.Extra


parseOptionalSpaces : Parser.Parser String
parseOptionalSpaces =
    Parser.oneOf
        [ Parser.map (always "") Parser.spaces
        , Parser.succeed ""
        ]


parseAnySupportedDate : Parser.Parser Time.Posix
parseAnySupportedDate =
    Parser.oneOf [ Parser.backtrackable parseIso8601Date, Parser.backtrackable parseEuropeanDateToPosix ]


parseIso8601Date : Parser.Parser Time.Posix
parseIso8601Date =
    Parser.succeed (\a b c -> a ++ "-" ++ b ++ "-" ++ c)
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.symbol "-"
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.symbol "-"
        |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |. Parser.end
        |> Parser.andThen parseIso8601DateToPosix


parseEuropeanDateToPosix : Parser.Parser Time.Posix
parseEuropeanDateToPosix =
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
        |> Parser.andThen parseEuropeanDateToISO8601String
        |> Parser.andThen parseIso8601DateToPosix


parseIso8601DateToPosix : String -> Parser.Parser Time.Posix
parseIso8601DateToPosix date =
    {- UTC Zone is hard coded for now -}
    case Time.Extra.fromIso8601Date Time.utc date of
        Just aPosix ->
            Parser.succeed aPosix

        Nothing ->
            Parser.problem "Invalid ISO8601 Encoding"


parseEuropeanDateToISO8601String : ( String, String, String ) -> Parser.Parser String
parseEuropeanDateToISO8601String ( day, month, year ) =
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


parseFloatLoop : Parser.Parser (List String)
parseFloatLoop =
    Parser.loop [] floatHelp


{-| @todo #1 Complete & Integrate this into the float parsing chain
-}
floatHelp : List String -> Parser.Parser (Parser.Step (List String) (List String))
floatHelp intList =
    Parser.oneOf
        [ Parser.succeed (\aInt -> Parser.Loop (aInt :: intList))
            |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
            |. Parser.symbol "."
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse intList))
        ]


{-| Used to Parse a float in a chain of other parsers. Supporting `,` _and_ `.` decimal separators.
@todo #1 Parsing Float Strings with additional thousands separators is not supported. The parsing will fail in this scenario. Consider using `Parser.loop` and `Parser.oneOf`. Also I think it should test if the created number is supported by the js runtime. There is an elm lib to do this. Maybe it is easy to implement it by yourself, and the lib is not needed. Also, should mixed separator 'types' be possible ? or could it cause a performance issue ? Right now it supports mixed decimal separators using `Parser.oneOf`
-}
parseChainFloat : Parser.Parser ( String, String, String )
parseChainFloat =
    Parser.succeed (\a b c -> ( a, b, c ))
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


{-| Parses Float Strings supporting `,` _and_ `.` decimal separators.
-}
parseFloat : Parser.Parser Float
parseFloat =
    Parser.succeed identity
        |= parseChainFloat
        |. Parser.end
        |> Parser.andThen convertToFloat


convertToFloat : ( String, String, String ) -> Parser.Parser Float
convertToFloat ( sign, first, last ) =
    case String.toFloat (sign ++ first ++ "." ++ last) of
        Just float ->
            Parser.succeed float

        Nothing ->
            Parser.problem "expecting float number"
