module UserTest exposing (suite)

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import WP.User as User exposing (User)


suite : Test
suite =
    describe "WP.User"
        [ describe "decoder (flags-side)"
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
            , test "defaults meta to empty dict when not present" <|
                \_ ->
                    case D.decodeValue User.decoder sampleJson of
                        Ok user ->
                            Expect.equal Dict.empty user.meta

                        Err err ->
                            Expect.fail (D.errorToString err)
            ]
        , describe "restDecoder"
            [ test "maps name -> displayName" <|
                \_ ->
                    case D.decodeValue (User.restDecoder User.defaultDecodeKeys) restDefaultJson of
                        Ok user ->
                            Expect.equal "Test User" user.displayName

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "fills missing roles/capabilities with empty lists (default context)" <|
                \_ ->
                    case D.decodeValue (User.restDecoder User.defaultDecodeKeys) restDefaultJson of
                        Ok user ->
                            Expect.all
                                [ \u -> Expect.equal [] u.roles
                                , \u -> Expect.equal [] u.capabilities
                                ]
                                user

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "extracts truthy capability keys from the REST capabilities object (edit context)" <|
                \_ ->
                    case D.decodeValue (User.restDecoder User.defaultDecodeKeys) restEditJson of
                        Ok user ->
                            Expect.all
                                [ \u -> Expect.equal [ "administrator" ] u.roles
                                , \u ->
                                    Expect.equal
                                        (List.sort [ "edit_posts", "manage_options" ])
                                        (List.sort u.capabilities)
                                ]
                                user

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "extracts requested user-meta keys" <|
                \_ ->
                    let
                        keys =
                            { meta = [ "_my_pref" ] }
                    in
                    case D.decodeValue (User.restDecoder keys) restWithMetaJson of
                        Ok user ->
                            Expect.equal
                                (Just "dark")
                                (Dict.get "_my_pref" user.meta
                                    |> Maybe.andThen (\v -> D.decodeValue D.string v |> Result.toMaybe)
                                )

                        Err err ->
                            Expect.fail (D.errorToString err)
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
        , describe "encodeBody"
            [ test "drops Nothing fields" <|
                \_ ->
                    let
                        body =
                            { emptyBody | username = Just "alice", email = Just "a@b.com" }
                    in
                    Expect.equal
                        "{\"username\":\"alice\",\"email\":\"a@b.com\"}"
                        (E.encode 0 (User.encodeBody body))
            , test "renames firstName/lastName to first_name/last_name" <|
                \_ ->
                    let
                        body =
                            { emptyBody | firstName = Just "Ada", lastName = Just "Lovelace" }

                        json =
                            E.encode 0 (User.encodeBody body)
                    in
                    Expect.all
                        [ \s -> Expect.equal True (String.contains "\"first_name\":\"Ada\"" s)
                        , \s -> Expect.equal True (String.contains "\"last_name\":\"Lovelace\"" s)
                        ]
                        json
            , test "wraps meta dict under a meta key" <|
                \_ ->
                    let
                        body =
                            { emptyBody | meta = Dict.fromList [ ( "_my_pref", E.string "dark" ) ] }
                    in
                    Expect.equal True
                        (String.contains "\"meta\":{\"_my_pref\":\"dark\"}" (E.encode 0 (User.encodeBody body)))
            ]
        ]


emptyBody : User.Body
emptyBody =
    User.emptyBody


sampleUser : User
sampleUser =
    { id = 7
    , displayName = "Test User"
    , roles = [ "editor" ]
    , capabilities = [ "edit_posts", "publish_posts" ]
    , meta = Dict.empty
    }


sampleJson : E.Value
sampleJson =
    E.object
        [ ( "id", E.int 7 )
        , ( "displayName", E.string "Test User" )
        , ( "roles", E.list E.string [ "editor" ] )
        , ( "capabilities", E.list E.string [ "edit_posts", "publish_posts" ] )
        ]


restDefaultJson : E.Value
restDefaultJson =
    E.object
        [ ( "id", E.int 7 )
        , ( "name", E.string "Test User" )
        , ( "url", E.string "" )
        , ( "description", E.string "" )
        , ( "link", E.string "https://example.test/author/test-user" )
        , ( "slug", E.string "test-user" )
        ]


restEditJson : E.Value
restEditJson =
    E.object
        [ ( "id", E.int 1 )
        , ( "name", E.string "Admin" )
        , ( "username", E.string "admin" )
        , ( "email", E.string "admin@example.test" )
        , ( "roles", E.list E.string [ "administrator" ] )
        , ( "capabilities"
          , E.object
                [ ( "edit_posts", E.bool True )
                , ( "manage_options", E.bool True )
                , ( "an_unused_cap", E.bool False )
                ]
          )
        ]


restWithMetaJson : E.Value
restWithMetaJson =
    E.object
        [ ( "id", E.int 1 )
        , ( "name", E.string "Admin" )
        , ( "meta", E.object [ ( "_my_pref", E.string "dark" ) ] )
        ]
