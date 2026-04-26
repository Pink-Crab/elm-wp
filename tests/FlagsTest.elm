module FlagsTest exposing (suite)

import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import WP.Flags as Flags


suite : Test
suite =
    describe "WP.Flags"
        [ describe "decoder"
            [ test "decodes a fully populated blob" <|
                \_ ->
                    case Flags.decode validBlob of
                        Ok flags ->
                            Expect.all
                                [ \f -> Expect.equal "https://example.test/wp-json/" f.restRoot
                                , \f -> Expect.equal "abc123" f.restNonce
                                , \f -> Expect.equal "wp/v2" f.restNamespace
                                , \f -> Expect.equal "https://example.test/wp-admin/admin-ajax.php" f.ajaxUrl
                                , \f -> Expect.equal "def456" f.ajaxNonce
                                , \f -> Expect.equal "demo-root" f.mountNode
                                , \f -> Expect.equal "en_GB" f.locale
                                ]
                                flags

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ D.errorToString err)
            , test "treats null currentUser as Nothing" <|
                \_ ->
                    case Flags.decode validBlob of
                        Ok flags ->
                            Expect.equal Nothing flags.currentUser

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "decodes currentUser when present" <|
                \_ ->
                    case Flags.decode blobWithUser of
                        Ok flags ->
                            case flags.currentUser of
                                Just user ->
                                    Expect.equal "Glynn" user.displayName

                                Nothing ->
                                    Expect.fail "currentUser should not be Nothing"

                        Err err ->
                            Expect.fail (D.errorToString err)
            , test "fails when a required field is missing" <|
                \_ ->
                    case Flags.decode (encodeBlob (\b -> { b | restNonce = Nothing })) of
                        Ok _ ->
                            Expect.fail "Decoder should have rejected missing restNonce"

                        Err _ ->
                            Expect.pass
            , test "preserves pluginData as a raw Value" <|
                \_ ->
                    case Flags.decode validBlob of
                        Ok flags ->
                            case D.decodeValue (D.field "demoNote" D.string) flags.pluginData of
                                Ok note ->
                                    Expect.equal "hello" note

                                Err err ->
                                    Expect.fail (D.errorToString err)

                        Err err ->
                            Expect.fail (D.errorToString err)
            ]
        ]


validBlob : E.Value
validBlob =
    encodeBlob identity


blobWithUser : E.Value
blobWithUser =
    encodeBlob
        (\b ->
            { b
                | currentUser =
                    Just
                        (E.object
                            [ ( "id", E.int 1 )
                            , ( "displayName", E.string "Glynn" )
                            , ( "roles", E.list E.string [ "administrator" ] )
                            , ( "capabilities", E.list E.string [ "manage_options" ] )
                            ]
                        )
            }
        )


type alias BlobBuilder =
    { restRoot : Maybe String
    , restNonce : Maybe String
    , restNamespace : Maybe String
    , ajaxUrl : Maybe String
    , ajaxNonce : Maybe String
    , mountNode : Maybe String
    , locale : Maybe String
    , currentUser : Maybe E.Value
    , pluginData : Maybe E.Value
    }


defaultBuilder : BlobBuilder
defaultBuilder =
    { restRoot = Just "https://example.test/wp-json/"
    , restNonce = Just "abc123"
    , restNamespace = Just "wp/v2"
    , ajaxUrl = Just "https://example.test/wp-admin/admin-ajax.php"
    , ajaxNonce = Just "def456"
    , mountNode = Just "demo-root"
    , locale = Just "en_GB"
    , currentUser = Nothing
    , pluginData = Just (E.object [ ( "demoNote", E.string "hello" ) ])
    }


encodeBlob : (BlobBuilder -> BlobBuilder) -> E.Value
encodeBlob f =
    let
        b =
            f defaultBuilder

        addField name maybeValue =
            case maybeValue of
                Just v ->
                    Just ( name, v )

                Nothing ->
                    Nothing
    in
    [ addField "restRoot" (Maybe.map E.string b.restRoot)
    , addField "restNonce" (Maybe.map E.string b.restNonce)
    , addField "restNamespace" (Maybe.map E.string b.restNamespace)
    , addField "ajaxUrl" (Maybe.map E.string b.ajaxUrl)
    , addField "ajaxNonce" (Maybe.map E.string b.ajaxNonce)
    , addField "mountNode" (Maybe.map E.string b.mountNode)
    , addField "locale" (Maybe.map E.string b.locale)
    , Just ( "currentUser", Maybe.withDefault E.null b.currentUser )
    , addField "pluginData" b.pluginData
    ]
        |> List.filterMap identity
        |> E.object
