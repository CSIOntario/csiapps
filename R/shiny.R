#' Wrapper UI for all Shiny apps, providing a consistent navbar and footer, and handling authentication status display.
#'
#' @param ... Additional UI elements to include in the main content area
#'
#' @return A Shiny UI object with a navbar, footer, and main content area
#' @export
ui_wrapper <- function(...) {
  tagList(
    tags$head(
      tags$script(HTML(
        "
      Shiny.addCustomMessageHandler('csip_redirect', function(url) {
        if (url && typeof url === 'string') {
          window.top.location.href = url;
        }
      });
      "
      )),
      tags$link(rel=" shortcut icon", href="https://csiontario.ca/wp-content/uploads/2022/04/cropped-CSIO-Favicon-192x192.png")
    ),
    navbar_ui(),
    fluidPage(
      shinyjs::useShinyjs(),
      style = "padding-bottom: 80px;",
      uiOutput("auth_status"),   # Signed in as X Y + Logout or redirect text
      # Application
      ...
    ),
    footer_ui()
  )
}

# -------------------------------------------------------------------
# Navbar, footer, profile card, tabs
# -------------------------------------------------------------------

navbar_ui <- function() {
  tags$nav(
    class = "navbar navbar-expand-lg navbar-dark bg-dark px-3",
    tags$div(
      class = "container-fluid",
      tags$a(
        class = "navbar-brand d-flex align-items-center",
        href = "#",
        tags$img(
          src = ifelse(
            package_state$INSTITUTE == "csipacific",
            "https://www.csipacific.ca/wp-content/uploads/2024/05/csi-pacific-logo-main.png",
            "https://csiontario.ca/wp-content/uploads/2022/03/logo-csi-ontario.png"
          ),
          height = "80px",
          style = "margin-right: 8px;"
        ),
        #tags$span(class = "h5 mb-0", "CSIP Apps")
      )
    )
  )
}

footer_ui <- function() {
  tags$footer(
    id = "footer",
    class = "mt-4 bg-dark text-white border-top border-light fixed-bottom",
    tags$div(
      class = "d-flex flex-wrap justify-content-between align-items-center py-3 container",
      tags$p(paste("\u00A9", format(Sys.Date(), "%Y"),
                   ifelse(package_state$INSTITUTE == "csipacific", "CSI Pacific", "CSI Ontario")
                   ), class = "col-md-4 mb-0"),
      tags$ul(class = "nav col-md-4 justify-content-end")
    )
  )
}


profile_card_server <- function(id, selected_profile_info) {
  moduleServer(id, function(input, output, session) {
    output$card <- renderUI({
      info <- selected_profile_info()
      if (is.null(info)) {
        return(div(class = "card shadow-sm border-0 p-3", "No profile selected."))
      }
      initials <- profile_initials(info$name)

      div(
        class = "card shadow-sm border-0",
        style = "max-width: 560px;",
        div(
          class = "row g-0 align-items-center",
          div(
            class = "col-auto p-3",
            div(
              initials,
              class = "d-flex align-items-center justify-content-center fw-semibold",
              style = paste(
                "width:64px;height:64px;border-radius:50%;",
                "background:#0d6efd;color:white;font-size:1.1rem;letter-spacing:0.02em;"
              )
            )
          ),
          div(
            class = "col ps-0",
            div(
              class = "card-body py-3",
              div(
                class = "d-flex flex-column flex-sm-row align-items-start align-items-sm-center gap-2",
                h5(class = "mb-1", info$name),
                span(class = "badge bg-secondary ms-0 ms-sm-2", info$role)
              ),
              div(
                class = "text-muted mt-1 d-flex align-items-center",
                tags$i(class = "bi bi-building me-2"),
                span(info$organization)
              )
            )
          )
        )
      )
    })
  })
}

profile_card_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("card"))
}

profile_initials <- function(name) {
  parts <- strsplit(name %||% "", "\\s+")[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return("?")
  paste0(toupper(substr(parts, 1, 1)))[1:min(2, length(parts))] |>
    paste(collapse = "")
}

profile_extract_card_info <- function(p) {
  person <- p$person %||% list()
  first  <- person$first_name %||% ""
  last   <- person$last_name  %||% ""
  name   <- trimws(paste(first, last))
  if (!nzchar(name)) name <- "-"

  role <- "-"
  if (!is.null(p$current_nomination) &&
      !is.null(p$current_nomination$role) &&
      !is.null(p$current_nomination$role$verbose_name)) {
    role <- p$current_nomination$role$verbose_name
  }

  org <- "-"
  if (!is.null(p$current_nomination) &&
      !is.null(p$current_nomination$organization) &&
      !is.null(p$current_nomination$organization$name)) {
    org <- p$current_nomination$organization$name
  }

  list(
    name = name,
    role = role,
    organization = org
  )
}

profile_build_label <- function(p) {
  person <- p$person %||% list()
  first  <- person$first_name %||% ""
  last   <- person$last_name  %||% ""
  label  <- trimws(paste(first, last))
  if (!nzchar(label)) label <- paste("Profile", p$id %||% "")
  label
}
