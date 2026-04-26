module WP.Notice exposing (Kind(..), success, error, info, warning, notice)

{-| Show admin notices via the JS bootstrap's `wpNotice` port.

The bootstrap routes these to `wp.data.dispatch('core/notices')` when the
WordPress data registry is available (admin pages), otherwise falls back to a
fixed-position toast injected into the DOM (frontend).

@docs Kind, success, error, info, warning, notice
-}

import Json.Encode as E
import WP.Ports


{-| -}
type Kind
    = Success
    | Error
    | Info
    | Warning


{-| -}
success : String -> Cmd msg
success =
    notice Success


{-| -}
error : String -> Cmd msg
error =
    notice Error


{-| -}
info : String -> Cmd msg
info =
    notice Info


{-| -}
warning : String -> Cmd msg
warning =
    notice Warning


{-| Lower-level — pick the kind explicitly. -}
notice : Kind -> String -> Cmd msg
notice kind message =
    WP.Ports.wpNotice
        (E.object
            [ ( "kind", E.string (kindToString kind) )
            , ( "message", E.string message )
            ]
        )


kindToString : Kind -> String
kindToString kind =
    case kind of
        Success ->
            "success"

        Error ->
            "error"

        Info ->
            "info"

        Warning ->
            "warning"
