module WP.Clipboard exposing (copy)

{-| Copy text to the user's clipboard via the JS bootstrap's `copyToClipboard`
port.

@docs copy
-}

import WP.Ports


{-| -}
copy : String -> Cmd msg
copy =
    WP.Ports.copyToClipboard
