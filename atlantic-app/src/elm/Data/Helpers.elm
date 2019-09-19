module Data.Helpers exposing (..)


maybeToBool : Maybe a -> Bool
maybeToBool aMaybe =
    case aMaybe of
        Just something ->
            True

        Nothing ->
            False


isResultOk : Result a b -> Bool
isResultOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
