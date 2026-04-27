module WP.Post exposing
    ( Post
    , DecodeKeys, defaultDecodeKeys
    , decoder, taxonomyOf, metaOf
    , Body, emptyBody, encodeBody
    , ListParams, defaultListParams
    , list, get, create, update, delete
    )

{-| Typed read/write of `/wp/v2/posts` (and any custom post type).

WordPress REST has two dynamic-schema dimensions on post objects:

  - **Taxonomies** — `categories`, `tags`, plus any custom taxonomies (e.g.
    `genre`, `recipe_cuisine`) appear as top-level fields on the post.
  - **Post meta** — keys registered via `register_post_meta(..., 'show_in_rest'
    => true)` appear inside a `meta` object.

Rather than hardcode either set, this module models both as dictionaries and
asks the caller which keys to extract via `DecodeKeys`. For the standard
`post` type with no registered meta, pass `{ taxonomies = ["categories",
"tags"], meta = [] }`. For a recipe CPT with custom taxonomies and meta keys,
pass `{ taxonomies = ["recipe_cuisine"], meta = ["_difficulty", "_servings"] }`.

The `Post` record extracts the *rendered* values for `title`, `content` and
`excerpt`. If you need the raw markup, fetch with `?context=edit` and decode
the response yourself via `WP.Http.request`.

@docs Post
@docs DecodeKeys, defaultDecodeKeys
@docs decoder, taxonomyOf, metaOf
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
type alias Post =
    { id : Int
    , date : String
    , slug : String
    , status : String
    , link : String
    , title : String
    , content : String
    , excerpt : String
    , author : Int
    , featuredMedia : Int
    , taxonomies : Dict String (List Int)
    , meta : Dict String D.Value
    }


{-| Tells the decoder which dynamic-schema fields to extract from the response.

`taxonomies` are top-level taxonomy keys (e.g. `["categories", "tags",
"genre"]`). Each becomes a `List Int` of term ids in `post.taxonomies`.

`meta` are registered post-meta keys (e.g. `["_my_field", "_another"]`). Each
becomes a `Json.Decode.Value` in `post.meta` — caller decodes the value into
its specific shape.
-}
type alias DecodeKeys =
    { taxonomies : List String
    , meta : List String
    }


{-| Empty `DecodeKeys` — no taxonomies, no meta extracted. -}
defaultDecodeKeys : DecodeKeys
defaultDecodeKeys =
    { taxonomies = [], meta = [] }


{-| Build a decoder using the given `DecodeKeys`. -}
decoder : DecodeKeys -> D.Decoder Post
decoder keys =
    D.succeed Post
        |> required "id" D.int
        |> required "date" D.string
        |> required "slug" D.string
        |> required "status" D.string
        |> required "link" D.string
        |> required "title" (D.field "rendered" D.string)
        |> required "content" (D.field "rendered" D.string)
        |> required "excerpt" (D.field "rendered" D.string)
        |> required "author" D.int
        |> optional "featured_media" D.int 0
        |> custom (taxonomiesDecoder keys.taxonomies)
        |> custom (metaDecoder keys.meta)


taxonomiesDecoder : List String -> D.Decoder (Dict String (List Int))
taxonomiesDecoder keys =
    let
        readKey key =
            D.maybe (D.field key (D.list D.int))
                |> D.map (\m -> ( key, Maybe.withDefault [] m ))
    in
    keys
        |> List.map readKey
        |> List.foldr (D.map2 (::)) (D.succeed [])
        |> D.map Dict.fromList


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


{-| Convenience: read a taxonomy's term ids from the post, defaulting to `[]`. -}
taxonomyOf : String -> Post -> List Int
taxonomyOf key post =
    Dict.get key post.taxonomies
        |> Maybe.withDefault []


{-| Convenience: read a post meta value as a raw `Value`, defaulting to `null`. -}
metaOf : String -> Post -> D.Value
metaOf key post =
    Dict.get key post.meta
        |> Maybe.withDefault E.null


{-| Body for create/update. All fields optional; only set what you want to
change. `taxonomies` and `meta` default to `Dict.empty`.

For `status`, valid values are `"publish"`, `"future"`, `"draft"`, `"pending"`,
`"private"`. WordPress will reject anything else.
-}
type alias Body =
    { title : Maybe String
    , content : Maybe String
    , excerpt : Maybe String
    , status : Maybe String
    , slug : Maybe String
    , author : Maybe Int
    , featuredMedia : Maybe Int
    , taxonomies : Dict String (List Int)
    , meta : Dict String E.Value
    }


{-| All-`Nothing` body — start here and use record-update to set fields. -}
emptyBody : Body
emptyBody =
    { title = Nothing
    , content = Nothing
    , excerpt = Nothing
    , status = Nothing
    , slug = Nothing
    , author = Nothing
    , featuredMedia = Nothing
    , taxonomies = Dict.empty
    , meta = Dict.empty
    }


{-| Encode a body into JSON suitable for POST. -}
encodeBody : Body -> E.Value
encodeBody body =
    let
        scalar =
            List.filterMap identity
                [ Maybe.map (\v -> ( "title", E.string v )) body.title
                , Maybe.map (\v -> ( "content", E.string v )) body.content
                , Maybe.map (\v -> ( "excerpt", E.string v )) body.excerpt
                , Maybe.map (\v -> ( "status", E.string v )) body.status
                , Maybe.map (\v -> ( "slug", E.string v )) body.slug
                , Maybe.map (\v -> ( "author", E.int v )) body.author
                , Maybe.map (\v -> ( "featured_media", E.int v )) body.featuredMedia
                ]

        taxonomies =
            body.taxonomies
                |> Dict.toList
                |> List.map (\( k, ids ) -> ( k, E.list E.int ids ))

        metaPair =
            if Dict.isEmpty body.meta then
                []

            else
                [ ( "meta", E.object (Dict.toList body.meta) ) ]
    in
    E.object (scalar ++ taxonomies ++ metaPair)


{-| Query params for `list`. `decodeKeys` controls what taxonomies and meta
the decoder extracts from each post in the response. `taxonomyFilters` filters
the post list by taxonomy term ids — e.g. `Dict.fromList [("categories",
[3])]` becomes `?categories=3`.
-}
type alias ListParams =
    { perPage : Int
    , page : Int
    , search : Maybe String
    , status : Maybe String
    , orderby : Maybe String
    , order : Maybe String
    , author : Maybe Int
    , decodeKeys : DecodeKeys
    , taxonomyFilters : Dict String (List Int)
    }


{-| 10 per page, page 1, no filters, no taxonomies/meta extracted. -}
defaultListParams : ListParams
defaultListParams =
    { perPage = 10
    , page = 1
    , search = Nothing
    , status = Nothing
    , orderby = Nothing
    , order = Nothing
    , author = Nothing
    , decodeKeys = defaultDecodeKeys
    , taxonomyFilters = Dict.empty
    }


{-| GET `/wp/v2/posts` with the given params. -}
list : Flags -> ListParams -> (Result WP.Http.Error (List Post) -> msg) -> Cmd msg
list flags params toMsg =
    WP.Http.get flags (listUrl params) (D.list (decoder params.decodeKeys)) toMsg


{-| GET `/wp/v2/posts/{id}`. -}
get : Flags -> Int -> DecodeKeys -> (Result WP.Http.Error Post -> msg) -> Cmd msg
get flags id keys toMsg =
    WP.Http.get flags ("/wp/v2/posts/" ++ String.fromInt id) (decoder keys) toMsg


{-| POST `/wp/v2/posts`. The keys control what's decoded out of the response. -}
create : Flags -> DecodeKeys -> Body -> (Result WP.Http.Error Post -> msg) -> Cmd msg
create flags keys body toMsg =
    WP.Http.post flags "/wp/v2/posts" (encodeBody body) (decoder keys) toMsg


{-| POST `/wp/v2/posts/{id}` for partial update. -}
update : Flags -> Int -> DecodeKeys -> Body -> (Result WP.Http.Error Post -> msg) -> Cmd msg
update flags id keys body toMsg =
    WP.Http.post flags ("/wp/v2/posts/" ++ String.fromInt id) (encodeBody body) (decoder keys) toMsg


{-| DELETE `/wp/v2/posts/{id}`. Returns the raw response body. -}
delete : Flags -> Int -> (Result WP.Http.Error D.Value -> msg) -> Cmd msg
delete flags id toMsg =
    WP.Http.delete flags ("/wp/v2/posts/" ++ String.fromInt id) D.value toMsg


listUrl : ListParams -> String
listUrl params =
    let
        baseParams =
            [ ( "per_page", Just (String.fromInt params.perPage) )
            , ( "page", Just (String.fromInt params.page) )
            , ( "search", params.search )
            , ( "status", params.status )
            , ( "orderby", params.orderby )
            , ( "order", params.order )
            , ( "author", Maybe.map String.fromInt params.author )
            ]

        taxFilters =
            params.taxonomyFilters
                |> Dict.toList
                |> List.map (\( k, ids ) -> ( k, Just (WP.Query.intList ids) ))
    in
    "/wp/v2/posts" ++ WP.Query.build (baseParams ++ taxFilters)
