module Data.Input exposing (Attributes)

import Html


type alias Attributes msg =
    List (Html.Attribute msg)
