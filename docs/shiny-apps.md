# Developing Shiny web applications

`csiapps` wraps an existing Shiny app with CSIAPPS authentication and a
consistent CSI navbar/footer. The same wrappers exist in both languages:
`ui_wrapper()` and `server_wrapper()`.

By default `csiapps` runs in **sandbox mode**, so a wrapped app can be developed
and run locally *without* any OAuth client credentials — the login redirect is
simulated (see [Sandbox mode](#sandbox-mode) below and the dedicated
[Sandbox mode](sandbox.md) article). The following environment variables are
only required for a real deployment, once sandbox mode is turned off:

- `CSIAPPS_CLIENT_ID` — client ID for the application registered in CSIAPPS.
- `CSIAPPS_CLIENT_SECRET` — client secret for the application.
- `CSIAPPS_REDIRECT_URI` — URL the application redirects to after authentication.
- `CSIAPPS_SCOPE` — (optional) scope of the authentication request.

## A starting app

Suppose you already have this Shiny app:

=== "R"

    ```r
    library(shiny)

    df <- faithful[, 2]

    ui <- fluidPage(
      titlePanel("Old Faithful Geyser Data"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
        ),
        mainPanel(plotOutput("distPlot"))
      )
    )

    server <- function(input, output) {
      output$distPlot <- renderPlot({
        bins <- seq(min(df), max(df), length.out = input$bins + 1)
        hist(df, breaks = bins, col = "darkgray", border = "white")
      })
    }

    shinyApp(ui = ui, server = server)
    ```

=== "Python"

    ```python
    from shiny import App, render, ui
    import numpy as np

    app_ui = ui.page_fluid(
        ui.panel_title("Old Faithful Geyser Data"),
        ui.layout_sidebar(
            ui.sidebar(ui.input_slider("bins", "Number of bins:", 1, 50, 30)),
            ui.output_plot("dist_plot"),
        ),
    )

    def server(input, output, session):
        @render.plot
        def dist_plot():
            import matplotlib.pyplot as plt
            fig, ax = plt.subplots()
            ax.hist(waiting, bins=input.bins(), color="darkgray", edgecolor="white")
            return fig

    app = App(app_ui, server)
    ```

To migrate this app into the CSIAPPS ecosystem, use the helpers below.

## 1. `set_institute()`

Specify which institute internal API calls (such as authentication redirects)
target:

=== "R"

    ```r
    # one of "csiontario" or "csipacific" (the default)
    set_institute("csiontario")
    ```

=== "Python"

    ```python
    # one of "csiontario" or "csipacific" (the default)
    csiapps.set_institute("csiontario")
    ```

## 2. `check_secrets()`

Confirm the required environment variables are set. Pass `verbose=True` to print
the values being checked (secrets masked). Outside sandbox mode it raises if a
required variable is missing, which makes it useful for debugging; in sandbox
mode it never raises and simply reports whether an access token was found.

=== "R"

    ```r
    check_secrets(verbose = FALSE)
    ```

=== "Python"

    ```python
    csiapps.check_secrets(verbose=False)
    ```

## 3. Sharing globals with the server

=== "R"

    R evaluates `ui` and `server` in separate scopes, so objects defined at the
    top level are not always visible inside the `server` function. Wrap that
    setup in `global_wrapper()` so it is evaluated in the global environment and
    reachable by internal helpers:

    ```r
    global_wrapper({
      df <- faithful[, 2]
    })
    ```

=== "Python"

    Python has no equivalent, and needs none: a module-level assignment in your
    `app.py` is already visible to the `server` function that closes over it.
    Just define it before `server`:

    ```python
    waiting = [...]   # module-level; the server closure sees it directly
    ```

    `global_wrapper()` is therefore **R-only** — see the
    [parity checklist](parity.md).

## 4. `ui_wrapper()` and `server_wrapper()`

Wrap the UI and server. These add the CSI navbar/footer, an auth-status line,
and the authentication flow (a real OAuth2 redirect in production, a simulated
login in sandbox mode).

=== "R"

    ```r
    # before
    shinyApp(ui = ui, server = server)

    # after
    shinyApp(ui = ui_wrapper(ui), server = server_wrapper(server))
    ```

=== "Python"

    ```python
    # before
    app = App(app_ui, server)

    # after
    app = App(csiapps.ui_wrapper(app_ui), csiapps.server_wrapper(server))
    ```

## Full migrated app

=== "R"

    ```r
    library(shiny)
    library(csiapps)

    # CSIAPPS setup
    set_institute("csiontario")
    check_secrets()

    global_wrapper({
      df <- faithful[, 2]
    })

    ui <- fluidPage(
      titlePanel("Old Faithful Geyser Data"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
        ),
        mainPanel(plotOutput("distPlot"))
      )
    )

    server <- function(input, output) {
      output$distPlot <- renderPlot({
        bins <- seq(min(df), max(df), length.out = input$bins + 1)
        hist(df, breaks = bins, col = "darkgray", border = "white")
      })
    }

    shinyApp(ui = ui_wrapper(ui), server = server_wrapper(server))
    ```

=== "Python"

    ```python
    from shiny import App, render, ui
    import csiapps

    # CSIAPPS setup
    csiapps.set_institute("csiontario")
    csiapps.check_secrets()

    app_ui = csiapps.ui_wrapper(
        ui.panel_title("Old Faithful Geyser Data"),
        ui.layout_sidebar(
            ui.sidebar(ui.input_slider("bins", "Number of bins:", 1, 50, 30)),
            ui.output_plot("dist_plot"),
        ),
    )

    def server(input, output, session):
        @render.plot
        def dist_plot():
            import matplotlib.pyplot as plt
            fig, ax = plt.subplots()
            ax.hist(waiting, bins=input.bins(), color="darkgray", edgecolor="white")
            return fig

    app = App(app_ui, csiapps.server_wrapper(server))
    ```

A runnable Python example is in
[`examples/app.py`](https://github.com/CSIOntario/csiapps-py/blob/main/examples/app.py)
(`shiny run --reload examples/app.py`).

## Waiting for login

In production, authentication is asynchronous: `server_wrapper()` redirects to
CSIAPPS, and the user's access token only becomes available once they return and
the token is exchanged. Any code that reads registration or warehouse data needs
that token, so it cannot run at app startup — it has to wait until login
completes.

`csiapps` handles this for you. The token is stored **per session** (never in a
process-global, so concurrent users never share one) and resolved reactively, so
the data helpers gate themselves:

- Called from a reactive context **before** login completes, `make_request()`,
  `fetch_org_options()`, `fetch_profiles()`, and `fetch_profile()` **cancel
  quietly** (they do not error) and the surrounding reactive **re-runs
  automatically** once the token arrives.
- Called outside a session (a script or the R console) with no token, they raise
  a clear "not authenticated" error.

The practical consequence: **fetch data from inside the server, not while
building the UI.** A call placed in the UI definition runs once at startup, when
no session and no token exist yet. Populate inputs from a reactive instead.

=== "R"

    ```r
    server <- function(input, output, session) {
      # Fetch org options once the token is available, then fill the dropdown.
      # make_request() gates itself, so this observer simply re-runs on login.
      org_list <- reactive({
        make_request("api/registration/organization")[["results"]]
      })
      observeEvent(org_list(), {
        updateSelectInput(session, "org",
                          choices = vapply(org_list(), `[[`, "", "name"))
      })
    }
    ```

=== "Python"

    ```python
    def server(input, output, session):
        # fetch_org_options() gates itself until login, so this effect just
        # re-runs when the token arrives. Start the input's choices empty.
        @reactive.effect
        def _load_orgs():
            ui.update_select("org", choices=csiapps.fetch_org_options())
    ```

### Gating your own work with `token_ready()`

If you have work that must wait for login but does **not** itself call a
`csiapps` data helper — for example, processing an uploaded file — guard it with
`token_ready()`. It returns whether a token is available and, read inside a
reactive context, re-fires the guard automatically once login completes.

=== "R"

    ```r
    processed <- reactive({
      req(token_ready())          # wait for login, then proceed
      req(input$upload)
      read.csv(input$upload$datapath)
    })
    ```

=== "Python"

    ```python
    @reactive.calc
    def processed():
        req(csiapps.token_ready())   # wait for login, then proceed
        req(input.upload())
        return pd.read_csv(input.upload()[0]["datapath"])
    ```

!!! note
    In sandbox mode the token comes from your environment at startup, so these
    guards pass immediately and the gating is invisible — it only matters once
    sandbox mode is off and a real, asynchronous login is involved.

## Sandbox mode

The code above is written for production, but you do **not** need client
credentials or a running identity provider to develop it. In sandbox mode (the
default) `server_wrapper()` **simulates the login redirect**: instead of sending
the browser to CSIAPPS to authenticate, it seeds the session from the
`CSIAPPS_ACCESS_TOKEN` already in your environment.

=== "R"

    ```r
    Sys.setenv(CSIAPPS_ACCESS_TOKEN = "your-dev-access-token")
    ```

=== "Python"

    ```python
    import os
    os.environ["CSIAPPS_ACCESS_TOKEN"] = "your-dev-access-token"
    ```

With that token set, the wrapped app runs locally and the header loads your
**real** identity (`/me`) from the API, exactly as after a production login. The
token is used **only to emulate the login** — everything else (sport
organizations, athletes, warehouse records) is dummy, served from the local
sandbox. If no token is set, the app shell still renders but shows an
unauthenticated notice.

!!! note
    Sandbox mode never touches real client data. Warehouse calls made through
    `make_request()` are emulated locally, and registration reads
    (`fetch_org_options()`, `fetch_profiles()`) come from the local dummy
    registry. Only `/me` uses your real token — call `set_institute()` to match
    the institute that issued it, or that lookup is rejected.

## Deploying

Because the app code is identical in both modes, deploying is just a matter of
turning sandbox mode off — no code changes required.

=== "R"

    ```r
    options(csiapps.sandbox = FALSE)   # or set CSIAPPS_ENV=production
    ```

    Then generate a `manifest.json` from the app directory so the deployment
    target (Posit Connect / shinyapps.io) can reproduce your package
    environment; regenerate it whenever dependencies change:

    ```r
    rsconnect::writeManifest()
    ```

=== "Python"

    ```python
    csiapps.set_sandbox_mode(False)   # or set CSIAPPS_ENV=production
    ```

    Deploy the app as you would any Shiny for Python app (for example
    `shiny run`, a container, or Posit Connect), with `CSIAPPS_ENV=production`
    and the OAuth client credentials set in the deployment environment.
