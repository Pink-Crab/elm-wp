port module WP.Ports exposing (wpNotice, copyToClipboard)

{-| Internal port declarations for `WP.Notice` and `WP.Clipboard`.

Not exposed in `elm.json` — consumers should not import this module directly,
use the typed APIs in `WP.Notice` / `WP.Clipboard` instead. The names here MUST
match the JS bootstrap; see the Contract in the package README.

@docs wpNotice, copyToClipboard
-}

import Json.Encode as E


{-| Outbound admin notice. Payload: `{ kind, message }`. -}
port wpNotice : E.Value -> Cmd msg


{-| Outbound clipboard text. -}
port copyToClipboard : String -> Cmd msg
