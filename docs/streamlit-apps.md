# Developing Streamlit web applications

`csiapps` adds CSI authentication, shared chrome, and token-aware API helpers to
an ordinary [Streamlit](https://docs.streamlit.io/) app. Streamlit support is
Python-only:

```bash
pip install 'csiapps[streamlit]'
```

The public wrapper is `page_wrapper(app_specific_logic)`. It authenticates the
visitor before invoking the supplied function, then renders the CSI navbar and
footer around it.

## Minimal app

Use one native Streamlit entrypoint:

```python
# app.py
import streamlit as st

import csiapps
from csiapps.streamlit import page_wrapper

csiapps.set_institute("csiontario")


def dashboard():
    st.title("Athletes")
    organisations = csiapps.fetch_org_options()
    organisation = st.selectbox(
        "Organisation",
        options=list(organisations),
        format_func=organisations.get,
        index=None,
    )
    if organisation is not None:
        profiles = csiapps.fetch_profiles(
            filters={"sport_org_id": int(organisation)}
        )
        st.dataframe(
            [csiapps.flatten_profile(profile) for profile in profiles],
            width="stretch",
        )


page_wrapper(dashboard)
```

Run it locally with:

```bash
streamlit run app.py
```

!!! danger "Keep protected work inside the callable"
    Streamlit executes top-level code before `page_wrapper()` is reached. Put
    protected API calls, sensitive calculations, and protected UI inside
    `dashboard`; only imports and non-sensitive configuration belong above it.

## Authentication design

CSI Access Portal exposes OAuth 2 authorization-code endpoints, not an OpenID
Connect discovery service. Streamlit's built-in `st.login()` is specifically
for OIDC, so the wrapper uses CSI OAuth directly.

In production the flow is:

1. `page_wrapper()` validates the OAuth configuration and the app's public URL.
2. It generates a PKCE verifier/challenge and a random browser nonce.
3. The page stores only that nonce in a ten-minute `SameSite=Lax` cookie and
   immediately sends the browser to CSI Access Portal. An existing CSI session
   continues without an extra click; otherwise CSI displays its login page.
4. CSI redirects to the app's own public URL with `code` and encrypted `state`
   query parameters.
5. The wrapper decrypts the AES-GCM state, checks its age, exact redirect URI,
   and browser nonce, then exchanges the code and PKCE verifier server-side.
6. The access token is kept only in that browser tab's server-side Streamlit
   session. `make_request()` and the `fetch_*()` helpers resolve it
   automatically.
7. The wrapper removes the OAuth parameters and reruns the page before invoking
   the dashboard.

The encrypted state can safely survive the full-page OAuth navigation without
putting the verifier in plaintext. The nonce cookie is JavaScript-readable only
so the trusted sign-in button can create it immediately before navigation; it
contains no access token, client secret, or identity. The delegated access
token is never placed in JavaScript, a URL, or browser storage.

Streamlit session state is tied to the WebSocket. A hard reload, browser
navigation, or reopened tab can therefore require sign-in again. This is an
intentional security trade-off: the package does not persist delegated access
tokens in cookies or local storage.

After an authentication error or an explicit logout, automatic authorization
is paused and the page leaves a **Sign in with CSI** button for a deliberate
retry. This prevents logout from immediately signing the visitor back in.

## Production variables

Set these as environment variables:

```text
CSIAPPS_ENV=production
CSIAPPS_CLIENT_ID=...
CSIAPPS_CLIENT_SECRET=...
CSIAPPS_REDIRECT_URI=https://your-published-streamlit-app.example/
CSIAPPS_SECRET_KEY=...
CSIAPPS_SCOPE=read write
```

`CSIAPPS_REDIRECT_URI` is the exact public app URL itself—there is no
`/redirect` callback route. It must:

- match the value registered with CSI Access Portal exactly;
- match the URL Streamlit sees for the current app, ignoring only a trailing
  slash and OAuth query parameters;
- use HTTPS, except that HTTP is accepted for `localhost`, `127.0.0.1`, and
  `::1`; and
- contain no credentials, query string, or fragment.

`CSIAPPS_SECRET_KEY` encrypts OAuth transaction state. Generate at least 32
random bytes, use a different key for each app, and keep it stable across
restarts and replicas:

```bash
python -c "import secrets; print(secrets.token_urlsafe(48))"
```

Do not commit either secret. Rotating `CSIAPPS_SECRET_KEY` invalidates only
sign-ins currently in progress; existing access tokens live in Streamlit
sessions and disappear when those sessions end.

## Sandbox mode

Sandbox mode remains the fail-safe default and skips OAuth entirely:

```bash
CSIAPPS_ENV=sandbox streamlit run app.py
```

Use `CSIAPPS_ACCESS_TOKEN` only when you deliberately want sandbox UI to
emulate an authenticated registration read. Sandbox data is process-local, so
use one worker when consistent dummy data matters.

## Posit Connect Cloud

This implementation is a native Streamlit app and is compatible with
[Posit Connect Cloud's Streamlit deployment
model](https://docs.posit.co/connect-cloud/how-to/python/streamlit.html):

1. Publish the repository as **Streamlit** content.
2. Select `app.py` as the primary file.
3. Add the production variables above as Connect Cloud secret environment
   variables. Use `os.environ`/`os.getenv`; Connect Cloud does not currently
   provide Streamlit's `st.secrets` file.
4. Publish once to obtain the final public URL, then set and register that exact
   URL as `CSIAPPS_REDIRECT_URI`. Republish after adding the variables.
5. Enable public/anonymous viewing at the Connect layer so visitors reach the
   CSI sign-in page instead of a second platform login.

No ASGI launcher, FastAPI content type, custom Streamlit server, sticky session,
or Connect API integration is required. Connect Cloud starts the app with its
supported native `streamlit run` workflow.

## Logout and navigation

The navbar's **Log out** link removes the token from the current Streamlit
session and returns to the sign-in screen. Navigating away or closing the
session also discards it. Because page navigation may reset session state,
multi-page apps should expect a fresh CSI sign-in when Streamlit creates a new
session.

## Testing and release canary

The package example is `examples/streamlit_app.py`. The full four-tab canary is
[`dummy-python-streamlit`](https://github.com/CSIOntario/dummy-python-streamlit).
It runs the sandbox suite locally and provides production tabs for the OAuth,
registration, and warehouse checks used before a package release.
