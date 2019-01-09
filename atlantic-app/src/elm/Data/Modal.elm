module Data.Modal exposing (Button, State, Title, Visibility(NotVisible, Visible))

import Data.Button
import Html exposing (Html)


type alias Button msg =
    ( Data.Button.Type, msg, Title )


type alias State msg =
    { visible : Visibility
    , content : Html msg {- TODO: don't do this .. adding html node into model, maybe replace with union type -}
    , buttons : List (Button msg)
    , title : Title
    }


type alias Title =
    String


type Visibility
    = NotVisible
    | Visible
