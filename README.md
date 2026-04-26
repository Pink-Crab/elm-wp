# Pink-Crab/elm-wp

Typed WordPress primitives for Elm apps that run inside WordPress — mounted by [`pinkcrab/elm-mount`](https://github.com/Pink-Crab/elm-mount) on the PHP side and booted by [`@pinkcrab/elm-wp-bootstrap`](https://github.com/Pink-Crab/elm-wp-bootstrap) on the JS side.

Part of a three-package set:

| Role | Package |
|------|---------|
| PHP mount helper | [`pinkcrab/elm-mount`](https://github.com/Pink-Crab/elm-mount) |
| JS bootstrap | [`@pinkcrab/elm-wp-bootstrap`](https://github.com/Pink-Crab/elm-wp-bootstrap) |
| Elm package (this package) | [`Pink-Crab/elm-wp`](https://github.com/Pink-Crab/elm-wp) |

## Install

```bash
elm install Pink-Crab/elm-wp
```

## Modules

### `WP.Flags`

Decoder for the JSON blob PHP injects on `window.<handle>`. Strict — decoding fails if PHP ever changes shape.

### `WP.User`

`User` record + `can` / `hasRole` helpers. Capabilities are a UI hint only; server-side permission callbacks remain the real authorisation gate.

### `WP.Http`

Typed REST helpers (`get`, `post`, `put`, `patch`, `delete`, plus a lower-level `request`) that go through `elm/http` with `flags.restNonce` attached as `X-WP-Nonce` automatically. Returns `Result Error a` so failures are exhaustive and pattern-matchable.

`WP.Http` does **not** route through the JS-side `wpApiFetch` port — it calls the WordPress REST API directly. If you specifically need `wp.apiFetch`'s middleware (REST embed normalisation, polling, etc.), declare your own port pair against the bootstrap-wired `wpApiFetch` / `wpApiFetchResult` instead.

### `WP.Notice`

`success` / `error` / `info` / `warning` (plus generic `notice`) for emitting admin notices. The JS bootstrap routes these to `wp.data.dispatch('core/notices')` when available, otherwise a fixed-position toast fallback.

### `WP.Clipboard`

`copy : String -> Cmd msg` for copy-to-clipboard via `navigator.clipboard.writeText` with a legacy `execCommand` fallback handled by the JS bootstrap.

## Hello, WordPress

A minimal app that decodes flags, fetches the current user from `/wp/v2/users/me`, and shows a notice on success.

```elm
module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import WP.Flags as Flags exposing (Flags)
import WP.Http
import WP.Notice


type alias UserMe =
    { name : String }


userMeDecoder : D.Decoder UserMe
userMeDecoder =
    D.map UserMe (D.field "name" D.string)


type Model
    = BadFlags D.Error
    | Ready { flags : Flags, me : Maybe UserMe }


type Msg
    = LoadMe
    | GotMe (Result WP.Http.Error UserMe)


init : D.Value -> ( Model, Cmd Msg )
init rawFlags =
    case Flags.decode rawFlags of
        Err err ->
            ( BadFlags err, Cmd.none )

        Ok flags ->
            ( Ready { flags = flags, me = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadMe, Ready state ) ->
            ( model, WP.Http.get state.flags "/wp/v2/users/me" userMeDecoder GotMe )

        ( GotMe (Ok me), Ready state ) ->
            ( Ready { state | me = Just me }
            , WP.Notice.success ("Loaded " ++ me.name)
            )

        ( GotMe (Err err), _ ) ->
            ( model, WP.Notice.error (WP.Http.errorToString err) )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        BadFlags err ->
            div [] [ text ("Bad flags: " ++ D.errorToString err) ]

        Ready { me } ->
            case me of
                Just u ->
                    div [] [ text ("Hi, " ++ u.name) ]

                Nothing ->
                    button [ onClick LoadMe ] [ text "Load me" ]


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

## Contract

This section is the **authoritative spec** shared by all three packages. Any change here is a contract bump and must be mirrored in the other two repos in lockstep.

### Flags blob

Emitted by the PHP side via `wp_localize_script( $handle, $handle, $blob )`. The JavaScript side reads it from `window.<handle>` and hands it to Elm as flags.

```json
{
  "restRoot":     "https://example.test/wp-json/",
  "restNonce":    "abc123...",
  "restNamespace":"wp/v2",
  "ajaxUrl":      "https://example.test/wp-admin/admin-ajax.php",
  "ajaxNonce":    "def456...",
  "mountNode":    "my_settings-root",
  "locale":       "en_GB",
  "currentUser": {
    "id":          1,
    "displayName": "Glynn Quelch",
    "roles":       ["administrator"],
    "capabilities":["manage_options", "edit_posts"]
  },
  "pluginData": {
    "pageTitle": "My Settings",
    "canEdit":   true
  }
}
```

Notes:
- `restNonce` is minted from the `wp_rest` action and is what `wp.apiFetch` needs.
- `ajaxNonce` is minted from a package-specific action (`elm_mount_<handle>`) for the legacy `admin-ajax.php` path.
- `pluginData` is the only free-form section — user-supplied flags via `->flags( [...] )` on the PHP side.
- `currentUser` is `null` when the visitor is logged out.
- `capabilities` is a UI hint for Elm to disable buttons etc; **never trust it for authorisation** (server-side checks are the real gate).

### Port names

The JS bootstrap and the Elm package must agree on these names. Changing any is a contract break.

| Direction | Port name | Purpose |
|-----------|-----------|---------|
| Elm → JS  | `wpApiFetch`       | Outbound REST call via `wp.apiFetch`. Payload: `{ id, method, path, body? }`. |
| JS → Elm  | `wpApiFetchResult` | Paired response. Payload: `{ id, ok, status, body }`. |
| Elm → JS  | `wpNotice`         | Show an admin notice. Payload: `{ kind: "success"\|"error"\|"info"\|"warning", message }`. |
| Elm → JS  | `copyToClipboard`  | Copy text to clipboard. Payload: `string`. |

`id` on `wpApiFetch` / `wpApiFetchResult` is a string correlation id the Elm side generates so multiple in-flight requests can be matched to their responses.

### Versioning

Elm's package convention starts at `1.0.0`, so this package begins its life at `1.x`. The PHP and npm siblings currently ride `0.x` during pre-release; compatibility across all three is tracked by a table in each README rather than strict version equality:

| `pinkcrab/elm-mount` | `@pinkcrab/elm-wp-bootstrap` | `Pink-Crab/elm-wp` |
|----------------------|------------------------------|--------------------|
| `0.1.x`              | `0.1.x`                      | `1.0.x`            |

## License

MIT © PinkCrab
