module WP.Query exposing (Param, build, intList, fromInt)

{-| Internal — URL query string builder shared by WP.Post / WP.Term / WP.User.

Each entry is `(key, Maybe value)` — `Nothing` is dropped, `Just v` becomes
`?key=encoded(v)`. Both keys and values are percent-encoded via `Url.percentEncode`.
-}

import Url


type alias Param =
    ( String, Maybe String )


build : List Param -> String
build params =
    let
        encoded =
            List.filterMap encodePair params
    in
    if List.isEmpty encoded then
        ""

    else
        "?" ++ String.join "&" encoded


encodePair : Param -> Maybe String
encodePair ( key, maybeValue ) =
    Maybe.map (\v -> Url.percentEncode key ++ "=" ++ Url.percentEncode v) maybeValue


intList : List Int -> String
intList xs =
    String.join "," (List.map String.fromInt xs)


fromInt : Int -> String
fromInt =
    String.fromInt
