module Helpers exposing (Padding(..), getPaddingClasses, isResultOk, maybeToBool)


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


type Padding
    = PaddingAll
    | PaddingLeftRight
    | PaddingTopBottom
    | PaddingTop
    | PaddingBottom
    | NoPadding


getPaddingClasses padding =
    case padding of
        NoPadding ->
            ""

        PaddingAll ->
            "uk-padding"

        PaddingLeftRight ->
            "uk-padding uk-padding-remove-top uk-padding-remove-bottom"

        PaddingTopBottom ->
            "uk-padding uk-padding-remove-left uk-padding-remove-right"

        PaddingTop ->
            "uk-padding uk-padding-remove-bottom uk-padding-remove-left uk-padding-remove-right"

        PaddingBottom ->
            "uk-padding uk-padding-remove-top uk-padding-remove-left uk-padding-remove-right"
