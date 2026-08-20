# Publishing authenticated Quarto documents

`csiapps-quarto` applies the existing CSI login to a Quarto HTML document or
dashboard and can generate a complete interactive snapshot for the signed-in
viewer. Posit Connect Cloud hosts the live document, but **CSI sign-in is the
only viewer authorization check**.

The live deployment uses `server: shiny`. The downloaded snapshot is a single
HTML file: it contains the registered session data and runs its filters, charts,
value cards, and tables entirely in the browser. It does not contact Connect
Cloud or a CSI data source after download.

## Install

Install the language-neutral extension in the Quarto project:

```bash
quarto add CSIOntario/csiapps-quarto
```

Install the matching adapter:

=== "R"

    ```r
    remotes::install_github("CSIOntario/csiapps-r")
    ```

=== "Python"

    ```bash
    pip install 'csiapps[quarto]'
    ```

Quarto 1.9 or newer is required.

## Configure the document

```yaml
---
title: "Athlete report"
format: dashboard # or html
server: shiny
filters:
  - csiapps
csiapps:
  institute: csiontario
  auth: csi
  snapshot:
    enabled: true
    filename: "{slug}-{timestamp}.html"
---
```

`auth: csi` without `server: shiny` is a build error. Use `auth: none` only for
a genuinely public/static document. Snapshot downloads are disabled unless
`snapshot.enabled` is true.

## Register the viewer's data

Call the adapter once from the document's server-executed code. The provider is
not evaluated until CSI login succeeds, and it runs in that viewer's session.

=== "R"

    ```r
    csiapps::quarto_setup(
      snapshot_data = function() {
        list(athletes = csiapps::fetch_profiles())
      }
    )
    ```

=== "Python"

    ```python
    import csiapps
    from csiapps.quarto import quarto_setup

    quarto_setup(
        snapshot_data=lambda: {
            "athletes": csiapps.fetch_profiles()
        }
    )
    ```

Each result must be a named tabular dataset. R accepts data frames or lists of
named records. Python accepts Pandas data frames or lists of dictionaries.
Missing values become JSON `null`; dates use ISO 8601; integers too large for
JavaScript are encoded as strings.

The adapter sends only the protected report body and registered data to the
browser. OAuth tokens, secrets, and data-source configuration remain on the
server.

## Add interactive components

Components are ordinary Quarto fenced divs, so their source is identical in R
and Python:

```markdown
::: {.csi-filter dataset="athletes" field="sport"
      type="multiselect" group="main" label="Sport"}
:::

::: {.csi-value dataset="athletes" operation="count"
      group="main" label="Athletes"}
:::

::: {.csi-chart dataset="athletes" type="scatter"
      x="age" y="score" group="main"}
:::

::: {.csi-table dataset="athletes"
      columns="name,sport,age,score" group="main"}
:::
```

Components with the same `dataset` and `group` share filters.

| Component | V1 options |
|---|---|
| `.csi-filter` | `select`, `multiselect`, `numeric-range`, `date-range` |
| `.csi-value` | `count`, `sum`, `mean`, `minimum`, `maximum` |
| `.csi-chart` | `bar`, `line`, `scatter`, `histogram` |
| `.csi-table` | Search, sort, pagination, declared columns |

Quarto prose, cards, tabs, pages, citations, and static images remain part of
the report. Mark server-only content `.csi-live-only`; the snapshot removes it.
When snapshots are enabled, detectable Shiny or HTML-widget output that is not
marked `.csi-live-only` stops the render with a validation error. Maps,
arbitrary widgets, and offline Shiny callbacks are not supported in V1.

## What the snapshot contains

When the viewer chooses **Download interactive snapshot**, the extension:

1. Captures the current filters, table state, and active tabs.
2. Includes every row of every registered session dataset, even rows currently
   hidden by filters.
3. Embeds the report, styles, images, runtime, data, and saved state in one HTML
   file.
4. Opens the file at the saved state while allowing filters to reset and expose
   the complete embedded data.

The snapshot is unencrypted and has no post-download authentication. Treat it
like any other complete data export and distribute it only to authorized
individuals. Generation fails rather than silently omitting data or a resource.

## Deploy to Connect Cloud

1. Publish the `.qmd` file (or `_quarto.yml`) as the primary file.
2. Include the installed `_extensions/csiapps` directory and the R
   `manifest.json` or Python `requirements.txt` in the deployment source.
3. Set `CSIAPPS_ENV=production` and the existing CSI OAuth client variables in
   Connect Cloud's secret Variables panel.
4. Register the deployed callback URL with CSIAPPS.
5. Enable **Public Access** in Connect Cloud. This makes the CSI login shell
   reachable; it does not reveal the report body or load data before CSI login.
6. Verify an anonymous visit receives only the login shell, CSI login reveals
   the report, and the downloaded snapshot works after the browser is offline.

The extension writes `.csiapps-protected-body.html` during Quarto rendering.
It is an internal application file, not a web asset. The deployment canary must
confirm that requesting this filename directly returns 404 before release.

Do not commit credentials or private datasets to the deployment repository.
Authentication protects the running document, not source files already placed
in a public Git repository.
