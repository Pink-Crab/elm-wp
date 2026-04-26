module WP.User exposing (User, decoder, can, hasRole)

{-| The current WordPress user, as emitted by `pinkcrab/elm-mount`.

`currentUser` on the flags blob is `null` when the visitor is logged out; use
`Maybe User` in your app's model rather than a bare `User`.

Capabilities and roles are exposed for UI hints only — **never rely on them for
authorisation**. Server-side permission callbacks (REST or admin) are the real
gate; this client-side snapshot is tamperable by anyone with devtools.

@docs User, decoder, can, hasRole
-}

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)


{-| -}
type alias User =
    { id : Int
    , displayName : String
    , roles : List String
    , capabilities : List String
    }


{-| Decodes the `currentUser` object from the flags blob. -}
decoder : D.Decoder User
decoder =
    D.succeed User
        |> required "id" D.int
        |> required "displayName" D.string
        |> required "roles" (D.list D.string)
        |> required "capabilities" (D.list D.string)


{-| True if the user has the named capability. UI hint only. -}
can : String -> User -> Bool
can capability user =
    List.member capability user.capabilities


{-| True if the user has the named role. UI hint only. -}
hasRole : String -> User -> Bool
hasRole role user =
    List.member role user.roles
