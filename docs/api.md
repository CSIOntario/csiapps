# Python API reference

The framework-independent API exported from the top-level `csiapps` Python
package, plus the Shiny app wrappers in the `csiapps.shiny` submodule (installed
with `pip install 'csiapps[shiny]'`). For the R package, see the
[R reference](https://csiontario.github.io/csiapps-r/reference/) (pkgdown); for
how the two map onto each other, see the [parity checklist](parity.md).

## Configuration

::: csiapps.config.set_institute
::: csiapps.config.is_sandbox_mode
::: csiapps.config.set_sandbox_mode

## Authentication

::: csiapps.auth.check_secrets

## Client

::: csiapps.client.make_request
::: csiapps.client.fetch_org_options
::: csiapps.client.fetch_profiles
::: csiapps.client.fetch_profile
::: csiapps.client.flatten_profile
::: csiapps.client.token_ready

## Sandbox

::: csiapps.sandbox.register_sandbox_schema
::: csiapps.sandbox.create_sport_org
::: csiapps.sandbox.create_profile
::: csiapps.sandbox.clear_sandbox
::: csiapps.sandbox.browse_sandbox

## App wrappers

Import these from `csiapps.shiny` (requires the `csiapps[shiny]` extra).

::: csiapps.shiny.ui_wrapper
::: csiapps.shiny.server_wrapper
