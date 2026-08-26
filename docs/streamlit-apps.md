# Developing Streamlit web applications

`csiapps` adds CSI authentication, shared chrome, and token-aware API helpers to
an ordinary [Streamlit](https://docs.streamlit.io/) app. Streamlit support is
Python-only. Local development uses sandbox mode; production uses the same app
code with real environment variables.

## Install

Install the optional Streamlit adapter:

```bash
pip install 'csiapps[streamlit]'
```

## Starting app

Suppose you already have this Streamlit app:

```python
# app.py
import streamlit as st


def dashboard():
    st.title("Athletes")
    st.write("Select an organisation to view its athletes.")


dashboard()
```

## Add the CSIAPPS wrapper

Import `page_wrapper()`, select the institute, and pass the app-specific
callable to the wrapper:

```python
import csiapps
from csiapps.streamlit import page_wrapper

csiapps.set_institute("csiontario")
page_wrapper(dashboard)
```

`page_wrapper()` authenticates the visitor before invoking `dashboard`, then
renders the CSI navbar and footer around it.

## Authentication and data access

Keep protected API calls, sensitive calculations, and protected UI inside the
callable. Streamlit executes top-level code before `page_wrapper()` is reached;
only imports and non-sensitive configuration belong above it.

In production, `page_wrapper()` performs the CSI OAuth authorization-code flow
with PKCE, validates the callback, and keeps the access token in the current
server-side Streamlit session. `make_request()` and the `fetch_*()` helpers then
resolve the token automatically:

```python
def dashboard():
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
```

CSI Access Portal exposes OAuth 2 endpoints rather than an OpenID Connect
discovery service, so Streamlit's OIDC-specific `st.login()` is not used. The
delegated token is never placed in JavaScript, a URL, or browser storage.

## Full app example

```python
# app.py
import streamlit as st

import csiapps
from csiapps.streamlit import page_wrapper

csiapps.set_institute("csiontario")
csiapps.check_secrets()

# Seed local data once. This block is skipped entirely in production.
if csiapps.is_sandbox_mode() and not csiapps.fetch_org_options():
    csiapps.create_sport_org("Rowing Canada", id=100)
    csiapps.create_profile(3, sport_org_id=100)


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

## Local development in sandbox mode

Sandbox mode is the fail-safe default and skips OAuth. Run the complete app
locally with:

```bash
export CSIAPPS_ENV=sandbox
export CSIAPPS_ACCESS_TOKEN=your-dev-access-token
streamlit run app.py
```

`CSIAPPS_ACCESS_TOKEN` is optional. Set it only when you want the header to show
your real `/me` identity; registration and warehouse data remain local dummy
data. The sandbox registry is process-local, so use one worker when consistent
dummy data matters. See [Sandbox mode](sandbox.md) for the complete local
registration and warehouse workflow.

## Production configuration

Set these values as secret environment variables:

```text
CSIAPPS_ENV=production
CSIAPPS_CLIENT_ID=...
CSIAPPS_CLIENT_SECRET=...
CSIAPPS_REDIRECT_URI=https://your-streamlit-app.example/
CSIAPPS_SECRET_KEY=...
CSIAPPS_SCOPE=read write
```

`CSIAPPS_REDIRECT_URI` is the exact public app URL itself—there is no
`/redirect` callback route. It must match the registered URL exactly, use HTTPS
outside local development, and contain no credentials, query string, or
fragment.

`CSIAPPS_SECRET_KEY` encrypts OAuth transaction state. Generate at least 32
random bytes, use a different key for each deployment, and keep it stable across
restarts and replicas:

```bash
python -c "import secrets; print(secrets.token_urlsafe(48))"
```

Do not commit either secret.

## Deploying

On Posit Connect Cloud:

1. Publish the project as **Streamlit** content.
2. Select `app.py` as the primary file.
3. Add the production variables above as secret environment variables.
4. Publish once to obtain the final public URL, then set and register that exact
   URL as `CSIAPPS_REDIRECT_URI`. Republish after adding the variables.
5. Enable public/anonymous viewing at the Connect layer so visitors reach the
   CSI sign-in page instead of a second platform login.

Connect Cloud starts the app through its supported native `streamlit run`
workflow. No custom ASGI launcher, FastAPI content type, or Connect API
integration is required.
