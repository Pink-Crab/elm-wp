module WP.Tax exposing
    ( Taxonomy
    , decoder
    , list, get
    )

{-| Read-only access to registered taxonomies via `/wp/v2/taxonomies`.

The REST API doesn't let you create taxonomies — they must be registered in
PHP via `register_taxonomy()`. This module just exposes the registry so Elm
apps can ask "what taxonomies apply to this post type?" or similar.

@docs Taxonomy, decoder, list, get
-}

import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import WP.Flags exposing (Flags)
import WP.Http


{-| -}
type alias Taxonomy =
    { name : String
    , slug : String
    , description : String
    , hierarchical : Bool
    , types : List String
    , restBase : String
    }


{-| -}
decoder : D.Decoder Taxonomy
decoder =
    D.succeed Taxonomy
        |> required "name" D.string
        |> required "slug" D.string
        |> optional "description" D.string ""
        |> required "hierarchical" D.bool
        |> required "types" (D.list D.string)
        |> required "rest_base" D.string


{-| GET `/wp/v2/taxonomies`. The endpoint returns an object keyed by taxonomy
slug; this list helper flattens it to a list of `Taxonomy` records. -}
list : Flags -> (Result WP.Http.Error (List Taxonomy) -> msg) -> Cmd msg
list flags toMsg =
    WP.Http.get flags "/wp/v2/taxonomies" listDecoder toMsg


{-| GET `/wp/v2/taxonomies/{slug}`. -}
get : Flags -> String -> (Result WP.Http.Error Taxonomy -> msg) -> Cmd msg
get flags slug toMsg =
    WP.Http.get flags ("/wp/v2/taxonomies/" ++ slug) decoder toMsg


listDecoder : D.Decoder (List Taxonomy)
listDecoder =
    D.keyValuePairs decoder
        |> D.map (List.map Tuple.second)
