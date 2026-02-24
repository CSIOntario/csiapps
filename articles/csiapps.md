# Developing Shiny Web Applications

### Preamble

The following environment variables must be set before developing a
shiny web application within the `CSIAPPS` ecosystem:

- `CSIAPPS_CLIENT_ID`: The client ID for the application registered in
  CSIAPPS.
- `CSIAPPS_CLIENT_SECRET`: The client secret for the application
  registered in CSIAPPS.
- `CSIAPPS_REDIRECT_URL`: The URL to which the application will redirect
  after authentication.
- `CSIAPPS_SCOPE`: (optional) The scope of the authentication request.

## Example

Suppose that you have the following shiny web application:

``` r
library(shiny)

df = faithful[, 2]

ui <- fluidPage(

    titlePanel("Old Faithful Geyser Data"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        bins <- seq(min(df), max(df), length.out = input$bins + 1)

        hist(df, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

shinyApp(ui = ui, server = server)
```

To migrate this app within the `CSIAPPS` ecosystem, we can leverage
several functions provided by `csiapps`.

### 1. `set_institute()`

We specify which institute internal API calls should be made for (such
as authentication redirects) by the application using
[`set_institute()`](https://csiontario.github.io/csiapps/reference/set_institute.md).

``` r
# institute can be set to one of "csiontario" or "csipacific"
csiapps::set_institute("csiontario")
```

### 2. `check_secrets()`

We then run
[`check_secrets()`](https://csiontario.github.io/csiapps/reference/check_secrets.md)
to ensure that all environment variables required have been set. The
`verbose` argument, which is `FALSE` by default, can be set to `TRUE` to
print out the values of the environment variables that are being
checked.
[`check_secrets()`](https://csiontario.github.io/csiapps/reference/check_secrets.md)
will throw an error if any of the required environment variables are not
present, making it useful for debugging.

``` r
csiapps::check_secrets(verbose = FALSE)
```

### 3. `global_wrapper()`

For code defined outside but used within the `server` function, we use
[`global_wrapper()`](https://csiontario.github.io/csiapps/reference/global_wrapper.md)
to ensure that it is accessible by internal helper functions.

``` r
csiapps::global_wrapper({
  df = faithful[, 2]
})
```

### 4. `ui_wrapper()` and `server_wrapper()`

The `ui` page and `server` function can simply be wrapped by
[`ui_wrapper()`](https://csiontario.github.io/csiapps/reference/ui_wrapper.md)
and
[`server_wrapper()`](https://csiontario.github.io/csiapps/reference/server_wrapper.md),
respectively. These convenience functions include additional code to
redirect the application to CSIAPPS for user authentication and provide
aesthetic formatting.

``` r
# original code

ui <- ...

server <- ...

shinyApp(ui = ui, server = server)

# wrapped code

shinyApp(ui = csiapps::ui_wrapper(ui), server = csiapps::server_wrapper(server))
```

### Summary

Therefore, the full code for the app, after migration, would look like
this:

``` r
library(shiny)
library(csiapps)

# CSIAPPS Setup
set_institute("csiontario")
check_secrets()

global_wrapper({
  df = faithful[, 2]
})

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Old Faithful Geyser Data"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        bins <- seq(min(df), max(df), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(df, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui_wrapper(ui), server = server_wrapper(server))
```
