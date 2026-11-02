# Developing Shiny web applications

`csiapps` wraps an existing Shiny app with CSIAPPS authentication and a
consistent CSI navbar/footer. The same wrappers exist in both languages:
`ui_wrapper()` and `server_wrapper()`.

By default `csiapps` runs in **sandbox mode**, so a wrapped app can be developed
locally without OAuth client credentials. Once the full app works locally, the
same code moves to production with real environment variables.

## Install

=== "R"

    ```r
    remotes::install_github("CSIOntario/csiapps-r")
    ```

=== "Python"

    Install the optional Shiny adapter and import the wrappers from its
    submodule:

    ```bash
    pip install 'csiapps[shiny]'
    ```

    ```python
    from csiapps.shiny import ui_wrapper, server_wrapper
    ```

## Starting app

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

    waiting = [43, 45, 50, 52, 56, 60, 65, 70, 75, 80]

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

To bring this app into the CSIAPPS ecosystem, add the wrapper calls below.

## Add the CSIAPPS wrapper

### 1. `set_institute()`

Specify which institute internal API calls (such as authentication redirects)
target:

=== "R"

    ```r
    # "csiontario", "csiatlantic", or "csipacific" (the default)
    set_institute("csiontario")
    ```

=== "Python"

    ```python
    # "csiontario", "csiatlantic", or "csipacific" (the default)
    csiapps.set_institute("csiontario")
    ```

### 2. `check_secrets()`

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

### 3. Sharing globals with the server

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

### 4. `ui_wrapper()` and `server_wrapper()`

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
    from csiapps.shiny import ui_wrapper, server_wrapper

    # before
    app = App(app_ui, server)

    # after
    app = App(ui_wrapper(app_ui), server_wrapper(server))
    ```

## Authentication and data access

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

In R 0.2.1, a browser session uses only its own token. Setting
`CSIAPPS_ACCESS_TOKEN` does not authenticate a signed-out production session;
the environment-token fallback is for scripts and the R console. The sandbox
wrapper can still explicitly seed a session from a developer token at startup.
If an API request rejects the session token with HTTP 401, the package clears
that token and offers sign-in again. Token-exchange failures also show a
recoverable authentication error. Apps should gate protected outputs on
`token_ready()` and clear cached data when authentication is lost.

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

## Full app example

=== "R"

    ```r
    library(shiny)
    library(csiapps)

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
    from csiapps.shiny import ui_wrapper, server_wrapper

    csiapps.set_institute("csiontario")
    csiapps.check_secrets()
    waiting = [43, 45, 50, 52, 56, 60, 65, 70, 75, 80]

    app_ui = ui_wrapper(
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

    app = App(app_ui, server_wrapper(server))
    ```

## Local development in sandbox mode

The code above is written for production, but you do **not** need client
credentials or a running identity provider to develop it. In sandbox mode (the
default) `server_wrapper()` **simulates the login redirect**: instead of sending
the browser to CSIAPPS to authenticate, it seeds the session from the
`CSIAPPS_ACCESS_TOKEN` already in your environment.

=== "R"

    Put local values in an ignored `.Renviron` file:

    ```text
    CSIAPPS_ENV=sandbox
    CSIAPPS_ACCESS_TOKEN=your-dev-access-token
    ```

    Then run `shiny::runApp()` from the app directory.

=== "Python"

    ```bash
    export CSIAPPS_ENV=sandbox
    export CSIAPPS_ACCESS_TOKEN=your-dev-access-token
    shiny run --reload app.py
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

## Production configuration

The app code does not change between environments. Set these variables in the
deployment environment:

- `CSIAPPS_ENV=production`
- `CSIAPPS_CLIENT_ID`
- `CSIAPPS_CLIENT_SECRET`
- `CSIAPPS_REDIRECT_URI`
- `CSIAPPS_SCOPE` (optional)

R 0.2.1 supports both confidential clients and public PKCE clients. Configure
`CSIAPPS_CLIENT_SECRET` for a confidential client; leave it unset or empty only
when the OAuth application is registered as a public client. Public clients
still require the client ID and registered redirect URI.

## Deploying

Because the app code is identical in both modes, deploying is just a matter of
turning sandbox mode off — no code changes required.

=== "R"

    Generate a `manifest.json` from the app directory so the deployment
    target (Posit Connect / shinyapps.io) can reproduce your package
    environment; regenerate it whenever dependencies change:

    ```r
    rsconnect::writeManifest()
    ```

=== "Python"

    Deploy the app as you would any Shiny for Python app (for example
    `shiny run`, a container, or Posit Connect), with `CSIAPPS_ENV=production`
    and the OAuth client credentials set in the deployment environment.
