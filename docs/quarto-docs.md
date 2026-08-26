# Publishing authenticated Quarto documents

`csiapps-quarto` applies the existing CSI login to an ordinary Quarto HTML
document or dashboard. Posit Connect Cloud hosts the document, while CSI
sign-in remains the viewer authorization check.

The core wrapper does three things:

1. Shows only the CSI login shell to anonymous visitors.
2. Reuses the existing per-session CSI authentication from `csiapps`.
3. Sends the protected Quarto report body to the browser after login.

Existing `csiapps` request, fetch, and ingestion functions remain the data
access layer. The Quarto wrapper does not introduce a second client.

## Install

Install the shared Quarto extension:

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
---
```

`auth: csi` requires `server: shiny`. Use `auth: none` only for a genuinely
public document.

## Attach authentication

=== "R"

    Put `server.R` beside the `.qmd` file:

    ```r
    function(input, output, session) {
      csiapps::quarto_setup()
    }
    ```

    Authenticated R server code must live in `server.R`. Inline
    `context: server` chunks are rejected because Quarto serializes their
    source into the rendered HTML.

=== "Python"

    Put this in a normal Python cell. Quarto runs normal Python cells once
    during rendering and again for each viewer session; `quarto_setup()` is a
    no-op during rendering.

    ```python
    from csiapps.quarto import quarto_setup

    quarto_setup()
    ```

After setup, use the existing `csiapps` functions from authenticated server
code just as you would in a Shiny application. Token resolution remains
session-aware; credentials and API configuration stay on the server.

## Author the report

Write ordinary Quarto Markdown, cards, tabs, pages, citations, static media,
and supported Shiny outputs. No CSI-specific chart, table, or filter syntax is
required for the core wrapper.

## Run locally

From the document directory, run:

```bash
quarto preview index.qmd
```

Use the URL printed by Quarto. Opening the generated HTML directly cannot run
the Shiny server or complete CSI authentication.

## Deploy to Connect Cloud

1. Publish `index.qmd` as the primary file.
2. Include `_extensions/csiapps` and the R `server.R` and `manifest.json`, or
   the Python `requirements.txt`, in the deployment source.
3. Set `CSIAPPS_ENV=production` and the existing CSI OAuth variables in
   Connect Cloud's secret Variables panel.
4. Register the deployed callback URL with CSIAPPS.
5. Enable **Public Access** so visitors can reach the CSI login shell.

The extension writes `.csiapps-protected-body.html` while rendering. It is an
internal application file, not a public web asset.

## Core production gate

Before release, verify both R and Python canaries:

- Anonymous visitors receive only the CSI login shell.
- The initial response contains no report body or protected data.
- CSI login reveals the protected document.
- Logout or session expiry removes the document and returns to sign-in.
- Two simultaneous viewers have independent sessions.
- A direct request for `.csiapps-protected-body.html` returns 404.
- The sandbox self-test board is green in both canaries.
- The live board exercises authenticated organisation/profile reads and reports
  a green ingest/retrieve round-trip when `CSIAPPS_TEST_SOURCE_UUID` is set.
- The Registry and Warehouse controls work inside the dynamically injected
  Quarto body, confirming that its Shiny inputs and outputs were rebound.

## Deferred: downloadable snapshots

Interactive HTML snapshot downloads are intentionally disabled in the core
release. They will be added and production-tested as a separate optional layer
after both authentication canaries pass.
