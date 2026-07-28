# Migrating Python apps to 0.3.0

Python **0.3.0** makes the web frameworks optional. Earlier versions installed
Shiny as a hard dependency and exported `ui_wrapper` / `server_wrapper` from the
top-level package, so a bare `pip install csiapps` was enough and
`from csiapps import ui_wrapper` worked. As of 0.3.0 the core (ingestion, auth,
the API client, and the sandbox) depends on **no** web framework, and each
framework's wrappers install and import separately.

!!! info "This page is Python-only"
    The **R** package is unaffected: `ui_wrapper()` and `server_wrapper()` are
    still exported directly, and there is no install extra to add. If your app is
    in R, there is nothing to change.

!!! warning "Breaking change"
    A pre-0.3 Shiny app will break on upgrade in two ways until you make the two
    edits below: `pip install csiapps` no longer installs Shiny, and
    `from csiapps import ui_wrapper, server_wrapper` no longer resolves.

## What you need to change

Exactly two things — the install, and the import. **Your app logic does not
change.**

| | Before (≤ 0.2.x) | After (0.3.0) |
|---|---|---|
| Install | `pip install csiapps` | `pip install 'csiapps[shiny]'` |
| Import the wrappers | `from csiapps import ui_wrapper, server_wrapper` | `from csiapps.shiny import ui_wrapper, server_wrapper` |
| Everything else (`make_request`, `fetch_*`, `token_ready`, sandbox helpers) | `from csiapps import ...` | **unchanged** |

### 1. Add the `shiny` extra to your dependencies

Wherever your app declares `csiapps`, add the `[shiny]` extra so Shiny is
installed alongside it.

=== "requirements.txt"

    ```diff
    - csiapps
    + csiapps[shiny]
    ```

    If you previously pinned Shiny yourself, you can drop that line and let the
    extra pull the right version:

    ```diff
    - csiapps
    - shiny
    + csiapps[shiny]
    ```

=== "pyproject.toml"

    ```diff
      dependencies = [
    -     "csiapps",
    +     "csiapps[shiny]",
      ]
    ```

=== "Posit Connect Cloud"

    Connect Cloud installs from your app's `requirements.txt` (or a pinned
    `manifest.json`). Update that file the same way — change `csiapps` to
    `csiapps[shiny]` — and redeploy so the build environment reinstalls with the
    extra.

### 2. Import the wrappers from `csiapps.shiny`

Only the two wrapper imports move. Change:

```diff
- from csiapps import ui_wrapper, server_wrapper
+ from csiapps.shiny import ui_wrapper, server_wrapper
```

If your code called them as attributes of the package
(`csiapps.ui_wrapper(...)`), import them from the submodule instead and call them
by name:

```diff
- app = App(csiapps.ui_wrapper(app_ui), csiapps.server_wrapper(server))
+ from csiapps.shiny import ui_wrapper, server_wrapper
+ app = App(ui_wrapper(app_ui), server_wrapper(server))
```

## Before and after

=== "Before (≤ 0.2.x)"

    ```python
    from shiny import App, render, ui
    import csiapps

    csiapps.set_institute("csiontario")

    app_ui = csiapps.ui_wrapper(
        ui.input_select("org", "Organisation", choices={}),
        ui.output_ui("athletes"),
    )

    def server(input, output, session):
        @reactive.effect
        def _load_orgs():
            ui.update_select("org", choices=csiapps.fetch_org_options())

    app = App(app_ui, csiapps.server_wrapper(server))
    ```

=== "After (0.3.0)"

    ```python
    from shiny import App, render, ui
    import csiapps
    from csiapps.shiny import ui_wrapper, server_wrapper

    csiapps.set_institute("csiontario")

    app_ui = ui_wrapper(
        ui.input_select("org", "Organisation", choices={}),
        ui.output_ui("athletes"),
    )

    def server(input, output, session):
        @reactive.effect
        def _load_orgs():
            ui.update_select("org", choices=csiapps.fetch_org_options())

    app = App(app_ui, server_wrapper(server))
    ```

Only the added import and the two call sites differ. The API client, the sandbox
helpers, the reactive login gating, and `token_ready()` all behave exactly as
before.

## What did *not* change

- **The core API.** `make_request`, `fetch_org_options`, `fetch_profiles`,
  `fetch_profile`, `flatten_profile`, `token_ready`, `set_institute`,
  `set_sandbox_mode`, `is_sandbox_mode`, and the sandbox CRUD helpers are all
  still imported from the top-level `csiapps` package, with identical behaviour.
- **Sandbox mode is still the default**, and the per-session token gating that
  lets `fetch_*()` wait for login is unchanged (see
  [Shiny apps → Waiting for login](shiny-apps.md#waiting-for-login)).
- **`check_secrets()` and the OAuth2 environment variables** are the same.

## Bonus: pure-ingestion scripts get lighter

A script that only ingests or reads warehouse data — no UI at all — no longer
pulls in a web framework. Keep the bare install:

```bash
pip install csiapps        # no Shiny, no Dash
```

```python
import csiapps

csiapps.set_sandbox_mode(False)
records = csiapps.make_request("api/warehouse/data-records",
                               query={"source_uuid": "my-source"})
```

This is a strict improvement over ≤ 0.2.x, where every install dragged in Shiny.

## Building a Dash app instead?

Dash has its own symmetric extra and submodule — `pip install 'csiapps[dash]'`
and `from csiapps.dash import attach, layout_wrapper`. The two frameworks are
mutually exclusive: a Shiny app installs only Shiny, a Dash app installs only
Dash, and neither is required by the core. See the
[Dash apps](dash-apps.md) tutorial to build one.

## Checklist

- [ ] Changed `csiapps` → `csiapps[shiny]` in every dependency manifest
      (`requirements.txt`, `pyproject.toml`, and any deployment/`manifest.json`).
- [ ] Changed `from csiapps import ui_wrapper, server_wrapper` →
      `from csiapps.shiny import ui_wrapper, server_wrapper`.
- [ ] Replaced any `csiapps.ui_wrapper(...)` / `csiapps.server_wrapper(...)`
      attribute calls with the submodule imports.
- [ ] Reinstalled (`pip install -e .` or `pip install -r requirements.txt`) and
      confirmed the app starts.
- [ ] Redeployed so the build environment picks up the `[shiny]` extra.
