module WP.Flags exposing (Flags, decoder, decode)

{-| The flags blob that `pinkcrab/elm-mount` localizes onto `window.<handle>`.

Call `decode` from your app's `init` to get a typed record out of the raw JSON
`Value` Elm receives as flags. Decoding is strict — if the PHP side ever
changes shape without updating this package, decoding fails loudly rather than
silently producing a stale model.

@docs Flags, decoder, decode
-}

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import WP.User as User exposing (User)


{-| Mirror of the PHP flags blob emitted by `Standard_Flags::build()` plus the
`pluginData` bag of app-specific flags.

`pluginData` is kept as a raw `D.Value` — your app supplies its own decoder for
the shape it expects.
-}
type alias Flags =
    { restRoot : String
    , restNonce : String
    , restNamespace : String
    , ajaxUrl : String
    , ajaxNonce : String
    , mountNode : String
    , locale : String
    , currentUser : Maybe User
    , pluginData : D.Value
    }


{-| Raw `Decoder Flags`. Prefer `decode` unless you want to compose. -}
decoder : D.Decoder Flags
decoder =
    D.succeed Flags
        |> required "restRoot" D.string
        |> required "restNonce" D.string
        |> required "restNamespace" D.string
        |> required "ajaxUrl" D.string
        |> required "ajaxNonce" D.string
        |> required "mountNode" D.string
        |> required "locale" D.string
        |> required "currentUser" (D.nullable User.decoder)
        |> required "pluginData" D.value


{-| Decodes the raw `Value` Elm receives as flags. -}
decode : D.Value -> Result D.Error Flags
decode =
    D.decodeValue decoder
