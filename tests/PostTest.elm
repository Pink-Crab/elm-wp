module PostTest exposing (suite)

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import WP.Post


suite : Test
suite =
    describe "WP.Post"
        [ describe "decoder"
            [ test "extracts rendered values for title/content/excerpt" <|
                \_ ->
                    case D.decodeValue (WP.Post.decoder WP.Post.defaultDecodeKeys) samplePostJson of
                        Ok post ->
                            Expect.all
                                [ \p -> Expect.equal "Hello world" p.title
                                , \p -> Expect.equal "<p>Body content.</p>" p.content
                                , \p -> Expect.equal "<p>Short.</p>" p.excerpt
                                ]
                                post

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "fills missing taxonomies with empty list" <|
                \_ ->
                    let
                        keys =
                            { taxonomies = [ "categories", "tags", "genre" ], meta = [] }
                    in
                    case D.decodeValue (WP.Post.decoder keys) samplePostJson of
                        Ok post ->
                            Expect.equal
                                (Dict.fromList
                                    [ ( "categories", [ 1, 2 ] )
                                    , ( "tags", [ 5 ] )
                                    , ( "genre", [] )
                                    ]
                                )
                                post.taxonomies

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "ignores taxonomies not in keys list" <|
                \_ ->
                    case D.decodeValue (WP.Post.decoder WP.Post.defaultDecodeKeys) samplePostJson of
                        Ok post ->
                            Expect.equal Dict.empty post.taxonomies

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "extracts requested meta keys" <|
                \_ ->
                    let
                        keys =
                            { taxonomies = [], meta = [ "_difficulty", "_servings" ] }
                    in
                    case D.decodeValue (WP.Post.decoder keys) samplePostJson of
                        Ok post ->
                            Expect.all
                                [ \p ->
                                    Expect.equal
                                        (Just "Easy")
                                        (Dict.get "_difficulty" p.meta
                                            |> Maybe.andThen
                                                (\v ->
                                                    D.decodeValue D.string v |> Result.toMaybe
                                                )
                                        )
                                , \p ->
                                    Expect.equal
                                        (Just 4)
                                        (Dict.get "_servings" p.meta
                                            |> Maybe.andThen
                                                (\v ->
                                                    D.decodeValue D.int v |> Result.toMaybe
                                                )
                                        )
                                ]
                                post

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "missing meta keys default to null" <|
                \_ ->
                    let
                        keys =
                            { taxonomies = [], meta = [ "_unset_key" ] }
                    in
                    case D.decodeValue (WP.Post.decoder keys) samplePostJson of
                        Ok post ->
                            Expect.equal
                                (Just E.null |> Maybe.map (E.encode 0))
                                (Dict.get "_unset_key" post.meta |> Maybe.map (E.encode 0))

                        Err err ->
                            Expect.fail (D.errorToString err)
            ]
        , describe "taxonomyOf"
            [ test "returns the list when present" <|
                \_ ->
                    let
                        post =
                            stubPost (Dict.fromList [ ( "categories", [ 7, 9 ] ) ]) Dict.empty
                    in
                    Expect.equal [ 7, 9 ] (WP.Post.taxonomyOf "categories" post)
            , test "returns empty list when missing" <|
                \_ ->
                    let
                        post =
                            stubPost Dict.empty Dict.empty
                    in
                    Expect.equal [] (WP.Post.taxonomyOf "missing" post)
            ]
        , describe "metaOf"
            [ test "returns null Value when missing" <|
                \_ ->
                    let
                        post =
                            stubPost Dict.empty Dict.empty
                    in
                    Expect.equal (E.encode 0 E.null) (E.encode 0 (WP.Post.metaOf "absent" post))
            ]
        , describe "encodeBody"
            [ test "drops Nothing fields" <|
                \_ ->
                    let
                        body =
                            { emptyBody | title = Just "Hello" }

                        expected =
                            E.object [ ( "title", E.string "Hello" ) ]
                    in
                    Expect.equal (E.encode 0 expected) (E.encode 0 (WP.Post.encodeBody body))
            , test "renames featuredMedia to featured_media" <|
                \_ ->
                    let
                        body =
                            { emptyBody | featuredMedia = Just 42 }
                    in
                    Expect.equal True
                        (String.contains "\"featured_media\":42" (E.encode 0 (WP.Post.encodeBody body)))
            , test "encodes taxonomies dict as top-level keys" <|
                \_ ->
                    let
                        body =
                            { emptyBody | taxonomies = Dict.fromList [ ( "genre", [ 1, 2 ] ) ] }
                    in
                    Expect.equal True
                        (String.contains "\"genre\":[1,2]" (E.encode 0 (WP.Post.encodeBody body)))
            , test "wraps meta dict under a meta key" <|
                \_ ->
                    let
                        body =
                            { emptyBody | meta = Dict.fromList [ ( "_difficulty", E.string "Easy" ) ] }
                    in
                    Expect.equal True
                        (String.contains "\"meta\":{\"_difficulty\":\"Easy\"}" (E.encode 0 (WP.Post.encodeBody body)))
            ]
        ]


emptyBody : WP.Post.Body
emptyBody =
    WP.Post.emptyBody


stubPost : Dict.Dict String (List Int) -> Dict.Dict String D.Value -> WP.Post.Post
stubPost taxonomies meta =
    { id = 1
    , date = "2026-01-01T00:00:00"
    , slug = "x"
    , status = "publish"
    , link = "https://example/x"
    , title = "x"
    , content = ""
    , excerpt = ""
    , author = 1
    , featuredMedia = 0
    , taxonomies = taxonomies
    , meta = meta
    }


samplePostJson : E.Value
samplePostJson =
    E.object
        [ ( "id", E.int 7 )
        , ( "date", E.string "2026-01-01T00:00:00" )
        , ( "slug", E.string "hello-world" )
        , ( "status", E.string "publish" )
        , ( "link", E.string "https://example.test/hello-world" )
        , ( "title", E.object [ ( "rendered", E.string "Hello world" ) ] )
        , ( "content", E.object [ ( "rendered", E.string "<p>Body content.</p>" ) ] )
        , ( "excerpt", E.object [ ( "rendered", E.string "<p>Short.</p>" ) ] )
        , ( "author", E.int 1 )
        , ( "featured_media", E.int 0 )
        , ( "categories", E.list E.int [ 1, 2 ] )
        , ( "tags", E.list E.int [ 5 ] )
        , ( "meta"
          , E.object
                [ ( "_difficulty", E.string "Easy" )
                , ( "_servings", E.int 4 )
                ]
          )
        ]
