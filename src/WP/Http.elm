module WP.Http exposing
    ( Error(..), errorToString
    , get, post, put, delete, patch
    , request
    )

{-| Typed REST helpers that go through `elm/http` with `flags.restNonce`
attached as `X-WP-Nonce` automatically.

These do **not** route through the JS-side `wpApiFetch` port — they call the
WordPress REST API directly via `elm/http`. This is simpler, type-safe, and
sufficient for the vast majority of uses.

If you specifically need `wp.apiFetch`'s middleware (REST embed normalisation,
polling helpers, etc.), declare your own port pair against the bootstrap-wired
`wpApiFetch` / `wpApiFetchResult` instead.

@docs Error, errorToString
@docs get, post, put, delete, patch
@docs request
-}

import Http
import Json.Decode as D
import Json.Encode as E
import WP.Flags exposing (Flags)


{-| All the ways a request can fail. `BadStatus` carries the response body so
you can decode it into a typed WP error if you want.
-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String


{-| Human-readable rendering. Useful for development; for user-facing copy you
probably want to pattern-match on `Error` and craft your own message.
-}
errorToString : Error -> String
errorToString err =
    case err of
        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "Request timed out"

        NetworkError ->
            "Network error"

        BadStatus status _ ->
            "Bad status: " ++ String.fromInt status

        BadBody reason ->
            "Bad response body: " ++ reason


{-| GET a JSON resource. -}
get : Flags -> String -> D.Decoder a -> (Result Error a -> msg) -> Cmd msg
get flags path decoder toMsg =
    request flags
        { method = "GET"
        , path = path
        , body = Http.emptyBody
        , decoder = decoder
        , toMsg = toMsg
        }


{-| POST JSON. -}
post : Flags -> String -> E.Value -> D.Decoder a -> (Result Error a -> msg) -> Cmd msg
post flags path body decoder toMsg =
    request flags
        { method = "POST"
        , path = path
        , body = Http.jsonBody body
        , decoder = decoder
        , toMsg = toMsg
        }


{-| PUT JSON. -}
put : Flags -> String -> E.Value -> D.Decoder a -> (Result Error a -> msg) -> Cmd msg
put flags path body decoder toMsg =
    request flags
        { method = "PUT"
        , path = path
        , body = Http.jsonBody body
        , decoder = decoder
        , toMsg = toMsg
        }


{-| PATCH JSON. -}
patch : Flags -> String -> E.Value -> D.Decoder a -> (Result Error a -> msg) -> Cmd msg
patch flags path body decoder toMsg =
    request flags
        { method = "PATCH"
        , path = path
        , body = Http.jsonBody body
        , decoder = decoder
        , toMsg = toMsg
        }


{-| DELETE. No request body by default. -}
delete : Flags -> String -> D.Decoder a -> (Result Error a -> msg) -> Cmd msg
delete flags path decoder toMsg =
    request flags
        { method = "DELETE"
        , path = path
        , body = Http.emptyBody
        , decoder = decoder
        , toMsg = toMsg
        }


{-| Lower-level escape hatch — choose your method and body shape. -}
request :
    Flags
    ->
        { method : String
        , path : String
        , body : Http.Body
        , decoder : D.Decoder a
        , toMsg : Result Error a -> msg
        }
    -> Cmd msg
request flags opts =
    Http.request
        { method = opts.method
        , headers = [ Http.header "X-WP-Nonce" flags.restNonce ]
        , url = absoluteUrl flags opts.path
        , body = opts.body
        , expect = Http.expectStringResponse opts.toMsg (resolve opts.decoder)
        , timeout = Nothing
        , tracker = Nothing
        }


resolve : D.Decoder a -> Http.Response String -> Result Error a
resolve decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ meta body ->
            Err (BadStatus meta.statusCode body)

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (BadBody (D.errorToString err))


absoluteUrl : Flags -> String -> String
absoluteUrl flags path =
    let
        root =
            trimTrailingSlash flags.restRoot

        prefixed =
            if String.startsWith "/" path then
                path

            else
                "/" ++ path
    in
    root ++ prefixed


trimTrailingSlash : String -> String
trimTrailingSlash s =
    if String.endsWith "/" s then
        String.dropRight 1 s

    else
        s
