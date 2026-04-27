module QueryTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import WP.Query


suite : Test
suite =
    describe "WP.Query"
        [ describe "build"
            [ test "empty params produces an empty string" <|
                \_ ->
                    Expect.equal "" (WP.Query.build [])
            , test "Nothing params are dropped" <|
                \_ ->
                    Expect.equal ""
                        (WP.Query.build
                            [ ( "search", Nothing )
                            , ( "status", Nothing )
                            ]
                        )
            , test "single Just param" <|
                \_ ->
                    Expect.equal "?per_page=10"
                        (WP.Query.build [ ( "per_page", Just "10" ) ])
            , test "multiple params joined with &" <|
                \_ ->
                    Expect.equal "?per_page=10&page=2"
                        (WP.Query.build
                            [ ( "per_page", Just "10" )
                            , ( "page", Just "2" )
                            ]
                        )
            , test "values are percent-encoded" <|
                \_ ->
                    Expect.equal "?search=hello%20world"
                        (WP.Query.build [ ( "search", Just "hello world" ) ])
            ]
        , describe "intList"
            [ test "joins ints with commas" <|
                \_ ->
                    Expect.equal "1,2,3" (WP.Query.intList [ 1, 2, 3 ])
            , test "empty list yields empty string" <|
                \_ ->
                    Expect.equal "" (WP.Query.intList [])
            ]
        ]
