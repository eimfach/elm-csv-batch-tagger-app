module Data.Button exposing (ActionType(..), Alignment(..), Size(..), Text, Type(..))


type ActionType
    = Add
    | Remove
    | NoActionType


type Size
    = Small
    | Medium
    | Large


type Alignment
    = Left
    | Right
    | NoAlign


type Type
    = Primary
    | Secondary
    | Default
    | Composed Type Size Alignment


type alias Text =
    String
