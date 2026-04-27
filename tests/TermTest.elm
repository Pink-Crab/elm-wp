module TermTest exposing (suite)

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import WP.Term


suite : Test
suite =
    describe "WP.Term"
        [ describe "decoder"
            [ test "decodes a category response" <|
                \_ ->
                    case D.decodeValue (WP.Term.decoder WP.Term.defaultDecodeKeys) categoryJson of
                        Ok term ->
                            Expect.all
                                [ \t -> Expect.equal 3 t.id
                                , \t -> Expect.equal "Category Three" t.name
                                , \t -> Expect.equal "category" t.taxonomy
                                , \t -> Expect.equal 12 t.count
                                , \t -> Expect.equal 0 t.parent
                                ]
                                term

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "decodes a tag response (no parent field)" <|
                \_ ->
                    case D.decodeValue (WP.Term.decoder WP.Term.defaultDecodeKeys) tagJson of
                        Ok term ->
                            Expect.all
                                [ \t -> Expect.equal 5 t.id
                                , \t -> Expect.equal "post_tag" t.taxonomy
                                , \t -> Expect.equal 0 t.parent
                                ]
                                term

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "fills missing description with empty string" <|
                \_ ->
                    case D.decodeValue (WP.Term.decoder WP.Term.defaultDecodeKeys) tagJson of
                        Ok term ->
                            Expect.equal "" term.description

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "extracts requested term-meta keys" <|
                \_ ->
                    let
                        keys =
                            { meta = [ "_color" ] }
                    in
                    case D.decodeValue (WP.Term.decoder keys) categoryWithMetaJson of
                        Ok term ->
                            Expect.equal
                                (Just "blue")
                                (Dict.get "_color" term.meta
                                    |> Maybe.andThen (\v -> D.decodeValue D.string v |> Result.toMaybe)
                                )

                        Err err ->
                            Expect.fail (D.errorToString err)
            ]
        , describe "encodeBody"
            [ test "drops Nothing fields" <|
                \_ ->
                    let
                        body =
                            { name = Just "New tag"
                            , description = Nothing
                            , slug = Nothing
                            , parent = Nothing
                            , meta = Dict.empty
                            }
                    in
                    Expect.equal "{\"name\":\"New tag\"}" (E.encode 0 (WP.Term.encodeBody body))
            , test "encodes parent as int" <|
                \_ ->
                    let
                        body =
                            { name = Just "Child"
                            , description = Nothing
                            , slug = Nothing
                            , parent = Just 42
                            , meta = Dict.empty
                            }
                    in
                    Expect.equal True
                        (String.contains "\"parent\":42" (E.encode 0 (WP.Term.encodeBody body)))
            , test "wraps meta dict under a meta key" <|
                \_ ->
                    let
                        body =
                            { name = Just "Genre"
                            , description = Nothing
                            , slug = Nothing
                            , parent = Nothing
                            , meta = Dict.fromList [ ( "_color", E.string "blue" ) ]
                            }
                    in
                    Expect.equal True
                        (String.contains "\"meta\":{\"_color\":\"blue\"}" (E.encode 0 (WP.Term.encodeBody body)))
            ]
        ]


categoryJson : E.Value
categoryJson =
    E.object
        [ ( "id", E.int 3 )
        , ( "count", E.int 12 )
        , ( "description", E.string "Things in cat 3" )
        , ( "link", E.string "https://example.test/category/c3" )
        , ( "name", E.string "Category Three" )
        , ( "slug", E.string "c3" )
        , ( "taxonomy", E.string "category" )
        , ( "parent", E.int 0 )
        ]


tagJson : E.Value
tagJson =
    E.object
        [ ( "id", E.int 5 )
        , ( "count", E.int 4 )
        , ( "link", E.string "https://example.test/tag/t5" )
        , ( "name", E.string "Tag Five" )
        , ( "slug", E.string "t5" )
        , ( "taxonomy", E.string "post_tag" )
        ]


categoryWithMetaJson : E.Value
categoryWithMetaJson =
    E.object
        [ ( "id", E.int 4 )
        , ( "count", E.int 1 )
        , ( "description", E.string "" )
        , ( "link", E.string "https://example.test/category/c4" )
        , ( "name", E.string "C Four" )
        , ( "slug", E.string "c4" )
        , ( "taxonomy", E.string "category" )
        , ( "parent", E.int 0 )
        , ( "meta", E.object [ ( "_color", E.string "blue" ) ] )
        ]
