module HttpTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import WP.Http exposing (Error(..))


suite : Test
suite =
    describe "WP.Http"
        [ describe "errorToString"
            [ test "BadUrl carries the url" <|
                \_ ->
                    Expect.equal "Bad URL: /broken" (WP.Http.errorToString (BadUrl "/broken"))
            , test "Timeout has a stable phrase" <|
                \_ ->
                    Expect.equal "Request timed out" (WP.Http.errorToString Timeout)
            , test "NetworkError has a stable phrase" <|
                \_ ->
                    Expect.equal "Network error" (WP.Http.errorToString NetworkError)
            , test "BadStatus reports the status code" <|
                \_ ->
                    Expect.equal "Bad status: 403" (WP.Http.errorToString (BadStatus 403 "{}"))
            , test "BadBody includes the underlying reason" <|
                \_ ->
                    Expect.equal
                        "Bad response body: parse failed"
                        (WP.Http.errorToString (BadBody "parse failed"))
            ]
        ]
