module WP.User exposing
    ( User
    , decoder
    , can, hasRole
    , DecodeKeys, defaultDecodeKeys
    , restDecoder, metaOf
    , Body, emptyBody, encodeBody
    , ListParams, defaultListParams
    , list, get, create, update, delete
    )

{-| WordPress users — both the current user (via flags) and the
`/wp/v2/users` REST collection.

The same `User` record covers both. The flags-side blob populates `roles`
and `capabilities` because we read them from PHP's `WP_User` directly. The
REST endpoint omits those fields under the default `view` context — they
only appear with `?context=edit`. Either way, the decoder fills missing
fields with empty lists rather than failing, so a flags-side user and a
REST-side user end up the same shape.

User meta registered via `register_user_meta(..., 'show_in_rest' => true)`
is exposed via the `meta` Dict — pass the keys you want extracted via
`DecodeKeys`. Flags-side currentUser always has `meta = Dict.empty` since
PHP-side `Standard_Flags` doesn't currently emit user meta.

Capabilities and roles are UI hints only — **never rely on them for
authorisation**. Server-side permission callbacks (REST or admin) are the
real gate; this client-side snapshot is tamperable.

@docs User
@docs decoder, can, hasRole
@docs DecodeKeys, defaultDecodeKeys
@docs restDecoder, metaOf
@docs Body, emptyBody, encodeBody
@docs ListParams, defaultListParams
@docs list, get, create, update, delete
-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as E
import WP.Flags exposing (Flags)
import WP.Http
import WP.Query


{-| -}
type alias User =
    { id : Int
    , displayName : String
    , roles : List String
    , capabilities : List String
    , meta : Dict String D.Value
    }


{-| Decoder for the `currentUser` object on the flags blob (PHP-side shape).

The same record shape lives in `WP.User.Type` (an internal module that
exists to break the WP.User → WP.Http → WP.Flags → WP.User import cycle).
The two definitions are structurally identical — Elm record aliases unify,
so `flags.currentUser` and `WP.User.User` are interchangeable.
-}
decoder : D.Decoder User
decoder =
    D.succeed User
        |> required "id" D.int
        |> required "displayName" D.string
        |> required "roles" (D.list D.string)
        |> required "capabilities" (D.list D.string)
        |> optional "meta" (D.dict D.value) Dict.empty


{-| Tells the decoder which registered user-meta keys to extract from the
response.
-}
type alias DecodeKeys =
    { meta : List String }


{-| Empty `DecodeKeys` — no meta extracted. -}
defaultDecodeKeys : DecodeKeys
defaultDecodeKeys =
    { meta = [] }


{-| Decoder for `/wp/v2/users` responses. Maps `name` → `displayName`,
fills `roles` / `capabilities` with empty lists when the response context
doesn't include them, and extracts the named meta keys.
-}
restDecoder : DecodeKeys -> D.Decoder User
restDecoder keys =
    D.succeed User
        |> required "id" D.int
        |> required "name" D.string
        |> optional "roles" (D.list D.string) []
        |> optional "capabilities" capabilitiesObjectDecoder []
        |> custom (metaDecoder keys.meta)


capabilitiesObjectDecoder : D.Decoder (List String)
capabilitiesObjectDecoder =
    D.keyValuePairs D.bool
        |> D.map (List.filter Tuple.second >> List.map Tuple.first)


metaDecoder : List String -> D.Decoder (Dict String D.Value)
metaDecoder keys =
    let
        readKey key =
            D.maybe (D.at [ "meta", key ] D.value)
                |> D.map (\m -> ( key, Maybe.withDefault E.null m ))
    in
    keys
        |> List.map readKey
        |> List.foldr (D.map2 (::)) (D.succeed [])
        |> D.map Dict.fromList


{-| Convenience: read a user meta value as a raw `Value`, defaulting to `null`. -}
metaOf : String -> User -> D.Value
metaOf key user =
    Dict.get key user.meta
        |> Maybe.withDefault E.null


{-| True if the user has the named capability. UI hint only. -}
can : String -> User -> Bool
can capability user =
    List.member capability user.capabilities


{-| True if the user has the named role. UI hint only. -}
hasRole : String -> User -> Bool
hasRole role user =
    List.member role user.roles


{-| Body for create/update.

`username`, `email`, `password` are required when creating but optional when
updating (you can update e.g. just the `name`).
-}
type alias Body =
    { username : Maybe String
    , email : Maybe String
    , password : Maybe String
    , name : Maybe String
    , firstName : Maybe String
    , lastName : Maybe String
    , slug : Maybe String
    , url : Maybe String
    , description : Maybe String
    , locale : Maybe String
    , roles : Maybe (List String)
    , meta : Dict String E.Value
    }


{-| All-`Nothing` body — start here and use record-update. -}
emptyBody : Body
emptyBody =
    { username = Nothing
    , email = Nothing
    , password = Nothing
    , name = Nothing
    , firstName = Nothing
    , lastName = Nothing
    , slug = Nothing
    , url = Nothing
    , description = Nothing
    , locale = Nothing
    , roles = Nothing
    , meta = Dict.empty
    }


{-| -}
encodeBody : Body -> E.Value
encodeBody body =
    let
        scalar =
            List.filterMap identity
                [ Maybe.map (\v -> ( "username", E.string v )) body.username
                , Maybe.map (\v -> ( "email", E.string v )) body.email
                , Maybe.map (\v -> ( "password", E.string v )) body.password
                , Maybe.map (\v -> ( "name", E.string v )) body.name
                , Maybe.map (\v -> ( "first_name", E.string v )) body.firstName
                , Maybe.map (\v -> ( "last_name", E.string v )) body.lastName
                , Maybe.map (\v -> ( "slug", E.string v )) body.slug
                , Maybe.map (\v -> ( "url", E.string v )) body.url
                , Maybe.map (\v -> ( "description", E.string v )) body.description
                , Maybe.map (\v -> ( "locale", E.string v )) body.locale
                , Maybe.map (\v -> ( "roles", E.list E.string v )) body.roles
                ]

        metaPair =
            if Dict.isEmpty body.meta then
                []

            else
                [ ( "meta", E.object (Dict.toList body.meta) ) ]
    in
    E.object (scalar ++ metaPair)


{-| -}
type alias ListParams =
    { perPage : Int
    , page : Int
    , search : Maybe String
    , orderby : Maybe String
    , order : Maybe String
    , roles : Maybe (List String)
    , who : Maybe String
    , decodeKeys : DecodeKeys
    }


{-| -}
defaultListParams : ListParams
defaultListParams =
    { perPage = 10
    , page = 1
    , search = Nothing
    , orderby = Nothing
    , order = Nothing
    , roles = Nothing
    , who = Nothing
    , decodeKeys = defaultDecodeKeys
    }


{-| GET `/wp/v2/users`. -}
list : Flags -> ListParams -> (Result WP.Http.Error (List User) -> msg) -> Cmd msg
list flags params toMsg =
    WP.Http.get flags (listUrl params) (D.list (restDecoder params.decodeKeys)) toMsg


{-| GET `/wp/v2/users/{id}`. -}
get : Flags -> Int -> DecodeKeys -> (Result WP.Http.Error User -> msg) -> Cmd msg
get flags id keys toMsg =
    WP.Http.get flags ("/wp/v2/users/" ++ String.fromInt id) (restDecoder keys) toMsg


{-| POST `/wp/v2/users`. -}
create : Flags -> DecodeKeys -> Body -> (Result WP.Http.Error User -> msg) -> Cmd msg
create flags keys body toMsg =
    WP.Http.post flags "/wp/v2/users" (encodeBody body) (restDecoder keys) toMsg


{-| POST `/wp/v2/users/{id}` for partial update. -}
update : Flags -> Int -> DecodeKeys -> Body -> (Result WP.Http.Error User -> msg) -> Cmd msg
update flags id keys body toMsg =
    WP.Http.post flags ("/wp/v2/users/" ++ String.fromInt id) (encodeBody body) (restDecoder keys) toMsg


{-| DELETE `/wp/v2/users/{id}?force=true&reassign={reassignId}`.

WordPress requires `force=true` (users don't go to trash) and `reassign`
(an int user id to receive any posts authored by the deleted user — pass
the deleted user's own id back to wipe their posts, or another user's id
to reassign). -}
delete : Flags -> Int -> Int -> (Result WP.Http.Error D.Value -> msg) -> Cmd msg
delete flags id reassignId toMsg =
    WP.Http.delete flags
        ("/wp/v2/users/"
            ++ String.fromInt id
            ++ "?force=true&reassign="
            ++ String.fromInt reassignId
        )
        D.value
        toMsg


listUrl : ListParams -> String
listUrl params =
    "/wp/v2/users"
        ++ WP.Query.build
            [ ( "per_page", Just (String.fromInt params.perPage) )
            , ( "page", Just (String.fromInt params.page) )
            , ( "search", params.search )
            , ( "orderby", params.orderby )
            , ( "order", params.order )
            , ( "roles", Maybe.map (String.join ",") params.roles )
            , ( "who", params.who )
            ]
