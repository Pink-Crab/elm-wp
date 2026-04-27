module WP.Term exposing
    ( Term
    , DecodeKeys, defaultDecodeKeys
    , decoder, metaOf
    , Body, emptyBody, encodeBody
    , ListParams, defaultListParams
    , list, get, create, update, delete
    )

{-| Typed read/write of taxonomy terms (`/wp/v2/categories`, `/wp/v2/tags`,
`/wp/v2/<custom>`).

All operations take a `restBase` argument identifying the taxonomy's REST
slug — `"categories"`, `"tags"`, or whatever the custom taxonomy registered.
That's the `rest_base` field from `WP.Tax`, **not** the taxonomy slug.

Term meta registered via `register_term_meta(..., 'show_in_rest' => true)`
is exposed via the `meta` Dict — pass the keys you want extracted via
`DecodeKeys`. See the `WP.Post` module docs for the same pattern.

@docs Term
@docs DecodeKeys, defaultDecodeKeys
@docs decoder, metaOf
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
type alias Term =
    { id : Int
    , count : Int
    , description : String
    , link : String
    , name : String
    , slug : String
    , taxonomy : String
    , parent : Int
    , meta : Dict String D.Value
    }


{-| Tells the decoder which registered term-meta keys to extract from the
response.
-}
type alias DecodeKeys =
    { meta : List String }


{-| Empty `DecodeKeys` — no meta extracted. -}
defaultDecodeKeys : DecodeKeys
defaultDecodeKeys =
    { meta = [] }


{-| -}
decoder : DecodeKeys -> D.Decoder Term
decoder keys =
    D.succeed Term
        |> required "id" D.int
        |> optional "count" D.int 0
        |> optional "description" D.string ""
        |> required "link" D.string
        |> required "name" D.string
        |> required "slug" D.string
        |> required "taxonomy" D.string
        |> optional "parent" D.int 0
        |> custom (metaDecoder keys.meta)


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


{-| Convenience: read a term meta value as a raw `Value`, defaulting to `null`. -}
metaOf : String -> Term -> D.Value
metaOf key term =
    Dict.get key term.meta
        |> Maybe.withDefault E.null


{-| Body for create/update. -}
type alias Body =
    { name : Maybe String
    , description : Maybe String
    , slug : Maybe String
    , parent : Maybe Int
    , meta : Dict String E.Value
    }


{-| -}
emptyBody : Body
emptyBody =
    { name = Nothing
    , description = Nothing
    , slug = Nothing
    , parent = Nothing
    , meta = Dict.empty
    }


{-| -}
encodeBody : Body -> E.Value
encodeBody body =
    let
        scalar =
            List.filterMap identity
                [ Maybe.map (\v -> ( "name", E.string v )) body.name
                , Maybe.map (\v -> ( "description", E.string v )) body.description
                , Maybe.map (\v -> ( "slug", E.string v )) body.slug
                , Maybe.map (\v -> ( "parent", E.int v )) body.parent
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
    , parent : Maybe Int
    , post : Maybe Int
    , hideEmpty : Maybe Bool
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
    , parent = Nothing
    , post = Nothing
    , hideEmpty = Nothing
    , decodeKeys = defaultDecodeKeys
    }


{-| GET `/wp/v2/{restBase}` with the given params. -}
list : Flags -> String -> ListParams -> (Result WP.Http.Error (List Term) -> msg) -> Cmd msg
list flags restBase params toMsg =
    WP.Http.get flags (listUrl restBase params) (D.list (decoder params.decodeKeys)) toMsg


{-| GET `/wp/v2/{restBase}/{id}`. -}
get : Flags -> String -> Int -> DecodeKeys -> (Result WP.Http.Error Term -> msg) -> Cmd msg
get flags restBase id keys toMsg =
    WP.Http.get flags (basePath restBase ++ "/" ++ String.fromInt id) (decoder keys) toMsg


{-| POST `/wp/v2/{restBase}`. -}
create : Flags -> String -> DecodeKeys -> Body -> (Result WP.Http.Error Term -> msg) -> Cmd msg
create flags restBase keys body toMsg =
    WP.Http.post flags (basePath restBase) (encodeBody body) (decoder keys) toMsg


{-| POST `/wp/v2/{restBase}/{id}`. -}
update : Flags -> String -> Int -> DecodeKeys -> Body -> (Result WP.Http.Error Term -> msg) -> Cmd msg
update flags restBase id keys body toMsg =
    WP.Http.post flags (basePath restBase ++ "/" ++ String.fromInt id) (encodeBody body) (decoder keys) toMsg


{-| DELETE `/wp/v2/{restBase}/{id}?force=true`. WordPress requires `force=true`
for term deletion (terms can't go to a trash). -}
delete : Flags -> String -> Int -> (Result WP.Http.Error D.Value -> msg) -> Cmd msg
delete flags restBase id toMsg =
    WP.Http.delete flags (basePath restBase ++ "/" ++ String.fromInt id ++ "?force=true") D.value toMsg


basePath : String -> String
basePath restBase =
    "/wp/v2/" ++ restBase


listUrl : String -> ListParams -> String
listUrl restBase params =
    basePath restBase
        ++ WP.Query.build
            [ ( "per_page", Just (String.fromInt params.perPage) )
            , ( "page", Just (String.fromInt params.page) )
            , ( "search", params.search )
            , ( "orderby", params.orderby )
            , ( "order", params.order )
            , ( "parent", Maybe.map String.fromInt params.parent )
            , ( "post", Maybe.map String.fromInt params.post )
            , ( "hide_empty"
              , Maybe.map
                    (\b ->
                        if b then
                            "true"

                        else
                            "false"
                    )
                    params.hideEmpty
              )
            ]
