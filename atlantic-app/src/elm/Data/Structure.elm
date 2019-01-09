module Data.Structure exposing (Bucket(Single, Multiple))

{-| data of type Bucket can be either a single instance of type `a`,
 or a List of type `a`
-}


type Bucket a
    = Single a
    | Multiple (List a)
