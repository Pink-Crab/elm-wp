module UserTest exposing (suite)

import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import WP.User as User exposing (User)


suite : Test
suite =
    describe "WP.User"
        [ describe "decoder"
            [ test "decodes a complete user object" <|
                \_ ->
                    case D.decodeValue User.decoder sampleJson of
                        Ok user ->
                            Expect.all
                                [ \u -> Expect.equal 7 u.id
                                , \u -> Expect.equal "Test User" u.displayName
                                , \u -> Expect.equal [ "editor" ] u.roles
                                , \u -> Expect.equal [ "edit_posts", "publish_posts" ] u.capabilities
                                ]
                                user

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "fails when a required field is missing" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "id", E.int 7 )
                                , ( "displayName", E.string "Test" )
                                , ( "roles", E.list E.string [] )
                                ]
                    in
                    case D.decodeValue User.decoder json of
                        Ok _ ->
                            Expect.fail "should have rejected missing capabilities"

                        Err _ ->
                            Expect.pass
            ]
        , describe "can"
            [ test "true when capability is present" <|
                \_ ->
                    Expect.equal True (User.can "edit_posts" sampleUser)
            , test "false when capability is absent" <|
                \_ ->
                    Expect.equal False (User.can "manage_options" sampleUser)
            ]
        , describe "hasRole"
            [ test "true when role is present" <|
                \_ ->
                    Expect.equal True (User.hasRole "editor" sampleUser)
            , test "false when role is absent" <|
                \_ ->
                    Expect.equal False (User.hasRole "administrator" sampleUser)
            ]
        ]


sampleUser : User
sampleUser =
    { id = 7
    , displayName = "Test User"
    , roles = [ "editor" ]
    , capabilities = [ "edit_posts", "publish_posts" ]
    }


sampleJson : E.Value
sampleJson =
    E.object
        [ ( "id", E.int 7 )
        , ( "displayName", E.string "Test User" )
        , ( "roles", E.list E.string [ "editor" ] )
        , ( "capabilities", E.list E.string [ "edit_posts", "publish_posts" ] )
        ]
