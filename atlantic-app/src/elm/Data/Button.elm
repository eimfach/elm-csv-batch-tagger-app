module Data.Button exposing (ActionType(..), Alignment(..), Size(..), Text, Type(..))


type ActionType {- TODO: rename this, it refers to an icon not an action  .. or does it ? -}
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
