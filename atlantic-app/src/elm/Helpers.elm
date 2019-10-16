module Helpers exposing (isResultOk, maybeToBool)


maybeToBool : Maybe a -> Bool
maybeToBool aMaybe =
    case aMaybe of
        Just _ ->
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
