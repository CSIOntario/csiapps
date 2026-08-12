# Developing Dash web applications

`csiapps` wraps an existing [Dash](https://dash.plotly.com/) app with CSIAPPS
authentication and a consistent CSI navbar/footer — the Dash counterpart of the
Shiny wrappers. The two entry points are `attach()` and `layout_wrapper()`.

!!! info "This page is Python-only"
    Dash is a Python framework, so it has no R equivalent — R apps use
    [Shiny](shiny-apps.md). Everything below is Python.

!!! note "Install the `dash` extra"
    Since **0.3.0** the Dash wrappers live in the `csiapps.dash` submodule behind
    an optional extra, symmetric with `csiapps[shiny]`:

    ```bash
    pip install 'csiapps[dash]'
    ```

    ```python
    from csiapps.dash import attach, layout_wrapper
    ```

    The web frameworks are independent extras — a Dash app installs only Dash,
    a Shiny app only Shiny, and a Streamlit app only Streamlit — while the core (`make_request`, the
    `fetch_*` helpers, the sandbox) imports neither.

By default `csiapps` runs in **sandbox mode**, so a wrapped app develops and runs
locally *without* any OAuth client credentials — the auth guard is skipped
entirely (see [Sandbox mode](#sandbox-mode) below and the dedicated
[Sandbox mode](sandbox.md) article). The following environment variables are
required only for a real deployment, once sandbox mode is off:

- `CSIAPPS_CLIENT_ID` — client ID for the application registered in CSIAPPS.
- `CSIAPPS_CLIENT_SECRET` — client secret for the application.
- `CSIAPPS_REDIRECT_URI` — the OAuth redirect URL; **must end in `/redirect`**
  (see [The redirect URI](#the-redirect-uri) below).
- `CSIAPPS_SCOPE` — (optional) scope of the authentication request.
- `CSIAPPS_SECRET_KEY` — signs the session cookie that holds the access token; a
  **Dash-specific** requirement (see [The session secret key](#the-session-secret-key)).

## A starting app

Suppose you already have this Dash app:

```python
from dash import Dash, dcc, html

app = Dash(__name__)

app.layout = html.Div([
    html.H3("Athletes"),
    dcc.Dropdown(id="org", placeholder="Organisation"),
])

if __name__ == "__main__":
    app.run(debug=True, port=8050)
```

To bring it into the CSIAPPS ecosystem you add two calls — `attach()` and
`layout_wrapper()` — plus the usual `set_institute()` / `check_secrets()` setup.

## 1. `set_institute()` and `check_secrets()`

Same as any csiapps app: choose the institute internal API calls target, then
fail fast on a misconfigured deployment.

```python
import csiapps

# "csiontario", "csiatlantic", or "csipacific" (the default)
csiapps.set_institute("csiontario")

# outside sandbox mode this raises if a required variable is missing
csiapps.check_secrets(verbose=True)
```

## 2. `attach()` — authentication and routes

Call `attach(app)` **once, immediately after constructing the app and before
assigning the layout**. In production it installs a `before_request` guard (via
`dash-auth`) that runs the OAuth2 PKCE flow, so no callback ever runs without a
token. In sandbox mode the guard is skipped entirely and the app runs with no
credentials and no network.

```python
from dash import Dash
from csiapps.dash import attach

app = Dash(__name__)
attach(app)
server = app.server   # WSGI entrypoint for gunicorn / Posit Connect
```

`attach()` also registers csiapps' own routes: the OAuth callback at `/redirect`,
`/csi-auth/logout`, and the chrome stylesheet at `/csi-auth/chrome.css`. In
production it additionally sets `Secure`/`HttpOnly`/`SameSite=Lax` on the session
cookie and applies `ProxyFix` (Posit Connect Cloud terminates TLS upstream).
Calling it twice on the same app is a no-op.

## 3. `layout_wrapper()` — the CSI chrome

Wrap your components in `layout_wrapper()` and assign the result to `app.layout`.
It adds the CSI navbar, footer, auth-status line, and (in sandbox mode) the
sandbox banner — the Dash counterpart of Shiny's `ui_wrapper()`.

```python
from dash import dcc, dash_table, html
from csiapps.dash import layout_wrapper

app.layout = layout_wrapper(
    html.H3("Athletes"),
    dcc.Dropdown(id="org", options=csiapps.fetch_org_options(), placeholder="Organisation"),
    dash_table.DataTable(id="people", page_size=10),
)
```

!!! warning "`layout_wrapper()` returns a callable, not a component"
    Dash calls it on **every page load**, which is what makes the signed-in name
    reflect the current user rather than whoever loaded the app first. Assign it
    straight to `app.layout`; because it is a callable and not a component, you
    cannot nest it inside another component — wrap the children instead. Pass
    `nav_links=[{"label": ..., "href": ...}]` to add navbar links for a
    multi-page (`use_pages`) app; a **Log out** link is appended automatically
    outside sandbox mode.

The chrome CSS is served by `attach()` and linked from the layout, so **`attach()`
must be called for the styling to load** — no `external_stylesheets`, no
`index_string` override, no `assets/` folder to vendor.

## No reactive login gating — the guard handles it

This is the key difference from Shiny. In a Shiny app, authentication is
asynchronous and the `fetch_*` helpers **gate themselves** reactively until the
token arrives (see [Shiny apps → Waiting for login](shiny-apps.md#waiting-for-login)).

In Dash there is nothing to gate. `attach()`'s `before_request` guard runs the
OAuth flow **before Dash renders anything**, so by the time any callback — or the
per-request `layout_wrapper` callable — executes, the user is already
authenticated and the token is in the Flask session. The `fetch_*` helpers pick
it up on their own:

```python
from dash import Input, Output, callback

@callback(Output("people", "data"), Input("org", "value"))
def load_people(org):
    # No token handling: behind attach()'s guard this callback only ever runs for
    # an authenticated user, and fetch_profiles() resolves the token itself.
    if not org:
        return []
    profiles = csiapps.fetch_profiles(filters={"sport_org_id": int(org)})
    return [csiapps.flatten_profile(p) for p in profiles]
```

Because the guard runs first, you can even call `fetch_org_options()` at layout
time (as in step 3): `layout_wrapper` is invoked per-request, inside the request
context, with the token already resolved. There is **no `dcc.Interval` login
bounce, no `dcc.Location`, and no manual `Authorization` header** — the pattern
older hand-rolled Dash apps needed.

!!! note "`token_ready()` still works"
    If you have work that must wait for login but does not itself call a
    `fetch_*` helper, `csiapps.token_ready()` reports whether a token is
    available (reading the Flask session inside a request). Behind the guard it
    is essentially always `True`, so most Dash apps never need it.

## The redirect URI

The OAuth provider sends the browser back to `CSIAPPS_REDIRECT_URI` after login,
and `attach()` handles that return on the **`/redirect`** route. So the value you
register in CSIAPPS **and** set in the environment must end in `/redirect`:

```bash
# local
CSIAPPS_REDIRECT_URI=http://127.0.0.1:8050/redirect
# deployed
CSIAPPS_REDIRECT_URI=https://your-app.example.ca/redirect
```

It must match the registered value **exactly** (OAuth providers match the
redirect URI by scheme, host, and path). This is the same path the other CSI
Dash apps register, so a migrating app keeps its registration unchanged.

## The session secret key

Dash stores the access token in a **Flask session cookie**, and `CSIAPPS_SECRET_KEY`
signs that cookie. Generate a different key for each deployed Dash app, then keep
that app's value **stable across restarts and workers**. It is not per-user or
per-session, and it is not a package-wide key. Reusing one key across unrelated
apps only increases the impact of a leak. `attach()` raises at startup in
production if the key is missing or shorter than 32 characters. Generate it once
per app:

```bash
python -c "import secrets; print(secrets.token_urlsafe(48))"
```

!!! info "Why Shiny doesn't need this"
    Shiny keeps the token server-side in the per-session `session$userData`, so
    there is no cookie to sign. Dash has no equivalent server-side session, hence
    the signed cookie and the key. It is not needed in sandbox mode.

## Full app

```python
import csiapps
from csiapps.dash import attach, layout_wrapper
from dash import Dash, Input, Output, callback, dash_table, dcc, html

csiapps.set_institute("csiontario")
csiapps.check_secrets(verbose=True)

app = Dash(__name__)
attach(app)
server = app.server   # for gunicorn / Posit Connect

app.layout = layout_wrapper(
    html.H3("Athletes"),
    dcc.Dropdown(id="org", options=csiapps.fetch_org_options(), placeholder="Organisation"),
    dash_table.DataTable(id="people", page_size=10),
)

@callback(Output("people", "data"), Input("org", "value"))
def load_people(org):
    if not org:
        return []
    profiles = csiapps.fetch_profiles(filters={"sport_org_id": int(org)})
    return [csiapps.flatten_profile(p) for p in profiles]

if __name__ == "__main__":
    app.run(debug=True, port=8050)
```

A runnable example is in
[`examples/dash_app.py`](https://github.com/CSIOntario/csiapps-py/blob/main/examples/dash_app.py)
(`python examples/dash_app.py`), and a full four-tab reference app — the Dash
twin of the Shiny canary — lives in
[`dummy-python-dash`](https://github.com/CSIOntario/dummy-python-dash).

## Sandbox mode

The code above is written for production, but you do **not** need client
credentials or a running identity provider to develop it. In sandbox mode (the
default) `attach()` **skips the auth guard entirely**, and the data comes from
the local dummy registry instead of the API. Seed it once at startup:

```python
import csiapps
from csiapps.dash import attach, layout_wrapper

csiapps.set_institute("csiontario")
csiapps.create_sport_org("Rowing Canada", id=100)
csiapps.create_profile(5, 100)

app = Dash(__name__)
attach(app)                        # sandbox: guard skipped, no credentials needed
app.layout = layout_wrapper(...)   # renders the sandbox banner + your components
```

Set `CSIAPPS_ACCESS_TOKEN` to have the header show your real `/me` identity;
otherwise the shell renders with an unauthenticated notice. Everything else —
orgs, athletes, warehouse records — is dummy, served from the local sandbox. The
full local warehouse/registration workflow is in the
[Sandbox mode](sandbox.md) article.

!!! warning "Sandbox deployments must run a single worker"
    The sandbox registry is **per-process**. A multi-worker sandbox deployment
    would serve a different dummy roster from each worker and lose records
    written to another, so deploy sandbox content with `WEB_CONCURRENCY=1`.
    `attach()` warns if it detects more than one worker in sandbox mode.
    Production is worker-count-agnostic (the token lives in the signed cookie,
    not process memory).

## Deploying

Because the app code is identical in both modes, deploying is just a matter of
turning sandbox mode off — no code changes required.

```python
csiapps.set_sandbox_mode(False)   # or set CSIAPPS_ENV=production
```

Deploy as any Dash/Flask app (gunicorn, a container, or Posit Connect), with the
OAuth client credentials, `CSIAPPS_SECRET_KEY`, and a registered
`CSIAPPS_REDIRECT_URI` set in the deployment environment. Expose the WSGI server
for the platform:

```python
server = app.server   # gunicorn app:server  /  Posit Connect entrypoint "app:server"
```

On Posit Connect the content type is **`python-dash`**; generate the manifest
with `rsconnect write-manifest dash .`.

!!! warning "Let CSIAPPS, not Posit, handle viewer authentication"
    In Posit Connect Cloud, enable **Public Access** for the content. This only
    removes Posit's viewer-login gate; `attach()` still protects the app and
    sends unauthenticated visitors to CSI Access Portal. If Public Access is
    disabled, visitors see a Posit login before the Dash process runs and the
    CSI OAuth flow cannot be tested. Use the app's stable public URL in
    `CSIAPPS_REDIRECT_URI`; a private-link URL is not a substitute for the
    registered OAuth redirect.
