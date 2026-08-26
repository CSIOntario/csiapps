# Publishing authenticated Quarto documents

`csiapps-quarto` adds the existing CSI login and shared CSI header/footer to an
ordinary Quarto HTML document or dashboard. The R and Python adapters reuse the
same token-aware registration and warehouse clients as the other application
wrappers.

Develop the complete document locally in sandbox mode, then use the same source
in production with real OAuth environment variables.

## Install

Install the shared Quarto extension in the project:

```bash
quarto add CSIOntario/csiapps-quarto
```

Install the matching language adapter:

=== "R"

    ```r
    remotes::install_github("CSIOntario/csiapps-r@v0.2.0")
    ```

=== "Python"

    ```bash
    pip install 'csiapps[quarto]'
    ```

Quarto 1.9 or newer is required.

## Starting document

Suppose you already have this server-backed Quarto dashboard:

```yaml
---
title: "Athlete report"
format: dashboard
server: shiny
---

## Athletes

Select an organisation to view its athletes.
```

## Add the CSIAPPS wrapper

Add the `csiapps` filter and configuration to the document YAML:

```yaml
---
title: "Athlete report"
format: dashboard
server: shiny
filters:
  - csiapps
csiapps:
  institute: csiontario
  auth: csi
---
```

Then attach the language adapter.

=== "R"

    Put `server.R` beside `index.qmd`:

    ```r
    function(input, output, session) {
      csiapps::quarto_setup()
    }
    ```

=== "Python"

    Put the setup call in a normal Python cell:

    ```python
    from csiapps.quarto import quarto_setup

    quarto_setup()
    ```

`auth: csi` requires `server: shiny`. Use `auth: none` only for a genuinely
public document.

## Authentication and data access

Before authentication, the browser receives only the CSI sign-in shell. After
login, `quarto_setup()` delivers the protected report body and makes the
per-viewer token available to existing functions such as `fetch_org_options()`,
`fetch_profiles()`, and `make_request()`.

R server code belongs in `server.R`. Do not use inline `context: server` chunks
for authenticated R work because Quarto serializes their source into the
rendered HTML. Python server cells run once during rendering and again for each
viewer session; `quarto_setup()` safely does nothing during the render pass.

Keep data access inside authenticated server or reactive code. Tokens,
credentials, and data-source configuration remain server-side.

## Full document example

The examples below produce the same organisation selector and athlete table in
R and Python.

=== "R"

    `index.qmd`:

    ```markdown
    ---
    title: "Athlete report"
    format: dashboard
    server: shiny
    engine: knitr
    filters:
      - csiapps
    csiapps:
      institute: csiontario
      auth: csi
    ---

    ## Athletes

    <div id="athlete_panel" class="shiny-html-output"></div>
    ```

    `server.R`:

    ```r
    library(shiny)
    library(csiapps)

    set_institute("csiontario")
    check_secrets()

    # Seed local data only in sandbox mode.
    if (is_sandbox_mode() && length(fetch_org_options()) == 0) {
      create_sport_org("Rowing Canada", id = 100)
      create_profile(3, sport_org_id = 100)
    }

    function(input, output, session) {
      quarto_setup()

      output$athlete_panel <- renderUI({
        if (!is_sandbox_mode()) req(token_ready())
        options <- fetch_org_options()
        choices <- setNames(
          vapply(options, function(x) as.character(x$value), character(1)),
          vapply(options, function(x) x$label, character(1))
        )
        tagList(
          selectInput("org", "Organisation", choices = choices),
          tableOutput("profiles_table")
        )
      })

      output$profiles_table <- renderTable({
        req(input$org)
        profiles <- fetch_profiles(
          filters = list(sport_org_id = as.integer(input$org))
        )
        if (length(profiles) == 0) return(data.frame())
        do.call(rbind, lapply(profiles, function(profile) {
          row <- flatten_profile(profile)
          as.data.frame(
            lapply(row, function(value) if (is.null(value)) NA else value),
            stringsAsFactors = FALSE
          )
        }))
      })
    }
    ```

=== "Python"

    A single `index.qmd` is sufficient:

    ````markdown
    ---
    title: "Athlete report"
    format: dashboard
    server: shiny
    jupyter: python3
    filters:
      - csiapps
    csiapps:
      institute: csiontario
      auth: csi
    ---

    ```{python}
    #| echo: false
    import pandas as pd
    from shiny import render, req, ui
    from shiny.session import get_current_session

    import csiapps
    from csiapps.quarto import quarto_setup

    csiapps.set_institute("csiontario")
    csiapps.check_secrets()
    quarto_setup()

    # Seed local data only in sandbox mode.
    if csiapps.is_sandbox_mode() and not csiapps.fetch_org_options():
        csiapps.create_sport_org("Rowing Canada", id=100)
        csiapps.create_profile(3, sport_org_id=100)

    if get_current_session() is not None:
        from shiny.express import input

        @render.ui
        def athlete_panel():
            if not csiapps.is_sandbox_mode():
                req(csiapps.token_ready())
            return ui.TagList(
                ui.input_select(
                    "org", "Organisation", choices=csiapps.fetch_org_options()
                ),
                ui.output_data_frame("profiles_table"),
            )

        @render.data_frame
        def profiles_table():
            req(input.org())
            profiles = csiapps.fetch_profiles(
                filters={"sport_org_id": int(input.org())}
            )
            rows = [csiapps.flatten_profile(profile) for profile in profiles]
            return render.DataGrid(pd.DataFrame(rows), width="100%")
    ```

    ## Athletes

    <div id="athlete_panel" class="shiny-html-output"></div>
    ````

## Local development in sandbox mode

Sandbox mode is the default. It skips OAuth and keeps registration and
warehouse data in the local process.

=== "R"

    Put local values in an ignored `.Renviron` file:

    ```text
    CSIAPPS_ENV=sandbox
    CSIAPPS_ACCESS_TOKEN=your-dev-access-token
    ```

=== "Python"

    Export local values in the shell that starts Quarto:

    ```bash
    export CSIAPPS_ENV=sandbox
    export CSIAPPS_ACCESS_TOKEN=your-dev-access-token
    ```

`CSIAPPS_ACCESS_TOKEN` is optional and is used only to show your real `/me`
identity in the header. The example seeds its own dummy organisation and
athletes. Use the same `create_*()`, `fetch_*()`, and `make_request()` functions
to exercise the complete local registration and warehouse workflow.

Start either document from its project directory:

```bash
quarto preview index.qmd
```

Open the URL printed by Quarto. Opening the generated HTML file directly cannot
run the Shiny server or the CSI wrapper.

## Production configuration

After the document works in sandbox mode, set these values as secret deployment
environment variables:

```text
CSIAPPS_ENV=production
CSIAPPS_CLIENT_ID=...
CSIAPPS_CLIENT_SECRET=...
CSIAPPS_REDIRECT_URI=https://your-quarto-document.example/
CSIAPPS_SCOPE=read write
```

Register the exact deployed URL as the redirect URI. Do not commit credentials
or private data to the document source.

## Deploying

On Posit Connect Cloud:

1. Publish `index.qmd` as the primary file.
2. Include `_extensions/csiapps` and, for R, `server.R` and `manifest.json`; for
   Python, include `requirements.txt`.
3. Add the production variables above as secret environment variables.
4. Register the final public URL with CSIAPPS and republish after configuration.
5. Enable **Public Access** so visitors can reach the CSI sign-in shell. CSI
   authentication still protects the document itself.

The extension writes `.csiapps-protected-body.html` while rendering. It is an
internal application file rather than a public web asset.
