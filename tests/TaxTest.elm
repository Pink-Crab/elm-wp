module TaxTest exposing (suite)

import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import WP.Tax


suite : Test
suite =
    describe "WP.Tax"
        [ describe "decoder"
            [ test "decodes a single taxonomy object" <|
                \_ ->
                    case D.decodeValue WP.Tax.decoder categoryTaxJson of
                        Ok tax ->
                            Expect.all
                                [ \t -> Expect.equal "Categories" t.name
                                , \t -> Expect.equal "category" t.slug
                                , \t -> Expect.equal True t.hierarchical
                                , \t -> Expect.equal "categories" t.restBase
                                , \t -> Expect.equal [ "post" ] t.types
                                ]
                                tax

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "fills missing description with empty string" <|
                \_ ->
                    case D.decodeValue WP.Tax.decoder minimalTaxJson of
                        Ok tax ->
                            Expect.equal "" tax.description

                        Err err ->
                            Expect.fail (D.errorToString err)
            ]
        ]


categoryTaxJson : E.Value
categoryTaxJson =
    E.object
        [ ( "name", E.string "Categories" )
        , ( "slug", E.string "category" )
        , ( "description", E.string "Default WP categories" )
        , ( "hierarchical", E.bool True )
        , ( "types", E.list E.string [ "post" ] )
        , ( "rest_base", E.string "categories" )
        ]


minimalTaxJson : E.Value
minimalTaxJson =
    E.object
        [ ( "name", E.string "Genres" )
        , ( "slug", E.string "genre" )
        , ( "hierarchical", E.bool False )
        , ( "types", E.list E.string [ "recipe" ] )
        , ( "rest_base", E.string "genres" )
        ]
