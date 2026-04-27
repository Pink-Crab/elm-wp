module WP.User.Type exposing (User, decoder)

{-| Internal — the `User` record + flags-side decoder shared between
`WP.User` and `WP.Flags`.

Lives in its own module to break the import cycle that would otherwise
exist (WP.User → WP.Http → WP.Flags → WP.User). Consumers should
`import WP.User exposing (User)` rather than importing this module
directly.

`meta` defaults to an empty `Dict` for flags-side users — `pinkcrab/elm-mount`'s
`Standard_Flags` doesn't currently emit user meta. REST responses populate
`meta` based on the `DecodeKeys` you pass to `WP.User.list`/`get`/`restDecoder`.

@docs User, decoder
-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)


{-| -}
type alias User =
    { id : Int
    , displayName : String
    , roles : List String
    , capabilities : List String
    , meta : Dict String D.Value
    }


{-| Decoder for the `currentUser` object on the flags blob (PHP-side shape).
`meta` is filled with `Dict.empty` since flags-side currentUser doesn't
include user meta. -}
decoder : D.Decoder User
decoder =
    D.succeed User
        |> required "id" D.int
        |> required "displayName" D.string
        |> required "roles" (D.list D.string)
        |> required "capabilities" (D.list D.string)
        |> optional "meta" (D.dict D.value) Dict.empty
