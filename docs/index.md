# csiapps

Helper functions and utilities for CSI data warehouse ingestion and
[Shiny](https://shiny.posit.co/), Dash, Streamlit, and authenticated interactive
Quarto documents. `csiapps` ships as **two
packages with full feature parity** — one for R, one for Python — so a team can
work in either language against the same CSIAPPS warehouse and registration API.

This site documents **both**: every tutorial shows the R and the Python code
side by side. Pick your language with the tabs and it stays selected across the
whole page.

The library has three layers:

- **API client** — `make_request`, `fetch_org_options`, `fetch_profiles`,
  `fetch_profile`, with OAuth2 PKCE auth.
- **Sandbox** — a local, in-memory emulation of the warehouse and registration
  API so you can develop the full schema → ingest → retrieve workflow with no
  network and no credentials. **Sandbox mode is on by default.**
- **App wrapper** — a consistent CSI navbar/footer plus authentication for a web
  app. In R that is `ui_wrapper` / `server_wrapper` for Shiny. In Python the
  wrappers live in per-framework submodules behind optional extras:
  `csiapps.shiny` (`ui_wrapper` / `server_wrapper`, `csiapps[shiny]`) and
  `csiapps.dash` (`attach` / `layout_wrapper`, `csiapps[dash]`) and
  `csiapps.streamlit` (`page_wrapper`, `csiapps[streamlit]`). Quarto uses the
  language-neutral `csiapps-quarto` extension plus `quarto_setup()` in R or
  `csiapps.quarto.quarto_setup()` in Python. The
  API client and sandbox above depend on no web framework; see
  [Migrating Python apps](migrating.md).

## Installation

=== "R"

    ```r
    # install.packages("remotes")
    remotes::install_github("CSIOntario/csiapps-r")
    ```

=== "Python"

    ```bash
    pip install csiapps            # core: ingestion + sandbox, no web framework
    pip install 'csiapps[shiny]'   # add the Shiny app wrappers
    pip install 'csiapps[dash]'    # add the Dash app wrappers
    pip install 'csiapps[streamlit]' # add the Streamlit app wrappers
    pip install 'csiapps[quarto]'    # add the Quarto Shiny adapter
    ```

    The web frameworks are independent optional extras; `import csiapps` pulls
    in none of them. Upgrading a pre-0.3 Shiny app? See
    [Migrating Python apps](migrating.md).

## Sandbox mode is the default

Every call routes to the local sandbox until you explicitly turn it off, so you
never hit the production warehouse by accident:

=== "R"

    ```r
    library(csiapps)

    is_sandbox_mode()   # TRUE

    # Turn it off for deployment:
    options(csiapps.sandbox = FALSE)   # or set CSIAPPS_ENV=production
    ```

=== "Python"

    ```python
    import csiapps

    csiapps.is_sandbox_mode()   # True

    # Turn it off for deployment:
    csiapps.set_sandbox_mode(False)          # or set CSIAPPS_ENV=production
    ```

## Where to go next

- **[Shiny apps](shiny-apps.md)** — wrap an app's UI and server with CSIAPPS
  authentication and chrome.
- **[Dash apps](dash-apps.md)** — the Python-only Dash equivalent, using
  `attach` / `layout_wrapper`.
- **[Streamlit apps](streamlit-apps.md)** — secure OAuth2 PKCE through a native
  Streamlit entrypoint using `page_wrapper`.
- **[Quarto documents](quarto-docs.md)** — publish a CSI-authenticated report to
  Connect Cloud and optionally download a complete interactive HTML snapshot.
- **[Migrating Python apps](migrating.md)** — upgrade a pre-0.3 Python app to the
  new `csiapps[shiny]` extra and `csiapps.shiny` imports.
- **[REST API](rest-api.md)** — `make_request` and the registration helpers.
- **[Sandbox mode](sandbox.md)** — the full local warehouse/registration
  workflow.
- **[Parity checklist](parity.md)** — how each R function maps to its Python
  equivalent.
- **[Releasing csiapps](releasing.md)** — for maintainers of the package itself:
  how to validate and publish a change.

## Function reference

- **Python:** [Python reference](api.md) (on this site).
- **R:** [R reference](https://csiontario.github.io/csiapps-r/reference/)
  (pkgdown).
