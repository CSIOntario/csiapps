
# ---- CSI APPS/Warehouse constants ----
SPORT_ORG_ENDPOINT <- "/api/registration/organization/"
PROFILE_ENDPOINT <- "/api/registration/profile/"

# environment variables
package_state <- new.env(parent = emptyenv())
package_state$INSTITUTE <- "csipacific"
SITE_URL <- function() paste0("https://apps.", package_state$INSTITUTE, ".ca")
CSIAPPS_AUTH_URL <- function() paste0(SITE_URL(), "/o/authorize/")
CSIAPPS_TOKEN_URL <- function() paste0(SITE_URL(), "/o/token/")
CSIAPPS_USERINFO_URL <- function() paste0(SITE_URL(), "/api/csiauth/me")

#' Set the target institute for API calls
#'
#' @param institute One of "csipacific" or "csiontario"
#'
#' @export
set_institute <- function(institute = "csipacific") {
  stopifnot(is.character(institute), length(institute) == 1, nzchar(institute),
            institute %in% c("csipacific", "csiontario"))

  package_state$INSTITUTE <- institute
}

clear_token <- function() {
  Sys.unsetenv("CSIAPPS_ACCESS_TOKEN")
}

#' Function to check that required environment variables for APPS authentication are set and valid
#'
#' @param verbose logical; if TRUE, prints the current values of relevant environment variables (masking secrets) to the console
#'
#' @export
check_secrets <- function(verbose = F) {

  bad <- character()
  if (!grepl("^https?://", CSIAPPS_AUTH_URL()))      bad <- c(bad, "CSIAPPS_AUTH_URL")
  if (!grepl("^https?://", CSIAPPS_TOKEN_URL()))     bad <- c(bad, "CSIAPPS_TOKEN_URL")
  if (!grepl("^https?://", Sys.getenv("CSIAPPS_REDIRECT_URI")))  bad <- c(bad, "CSIAPPS_REDIRECT_URI")
  if (length(bad) > 0) stop("Invalid or missing URL env vars: ", paste(bad, collapse = ", "))

  if (verbose) {

    message("AUTH_URL: '", CSIAPPS_AUTH_URL(), "'  REDIRECT_URI: '", Sys.getenv("CSIAPPS_REDIRECT_URI"), "'")

    env_dump <- list(
      CSIAPPS_CLIENT_ID          = Sys.getenv("CSIAPPS_CLIENT_ID"),
      CSIAPPS_CLIENT_SECRET_SET  = nzchar(Sys.getenv("CSIAPPS_CLIENT_SECRET")),
      CSIAPPS_AUTH_URL           = CSIAPPS_AUTH_URL(),
      CSIAPPS_TOKEN_URL          = CSIAPPS_TOKEN_URL(),
      CSIAPPS_REDIRECT_URI       = Sys.getenv("CSIAPPS_REDIRECT_URI"),
      CSIAPPS_SCOPE              = Sys.getenv("CSIAPPS_SCOPE", "read write"),
      CSIAPPS_USERINFO_URL       = CSIAPPS_USERINFO_URL()
    )
    message("CSIAPPS environment on startup:")
    utils::str(env_dump)
  }
}

# -------------------------------------------------------------------
# Registration API helpers
# -------------------------------------------------------------------

flatten_record <- function(rec) {
  data <- rec$data %||% list()

  record_identifier <- rec$uuid %||% rec$id
  subject           <- rec$subject
  subject_label     <- "-"
  if (!is.null(subject)) {
    fn <- subject$first_name %||% ""
    ln <- subject$last_name  %||% ""
    subject_label <- trimws(paste(fn, ln))
    if (!nzchar(subject_label)) subject_label <- "-"
  }

  list(
    id             = record_identifier,
    dataset_uuid   = rec$dataset_uuid %||% NA,
    profile        = subject_label,
    created_at     = rec$created_at %||% NA,
    updated_at     = rec$updated_at %||% NA,
    sport          = rec$subject$sport$name %||% NA,
    data           = data
  )
}

fetch_profiles_api <- function(token = NULL, filters = list()) {
  if (is.null(token) || !nzchar(token)) {
    token <- Sys.getenv("CSIAPPS_ACCESS_TOKEN")
  }
  if (!nzchar(token)) {
    stop("fetch_profiles_api: no CSIAPPS_ACCESS_TOKEN set; user not authenticated?")
  }

  url    <- paste0(SITE_URL(), PROFILE_ENDPOINT)  # "/api/registration/profile/"
  params <- c(filters, list(limit = 100L, offset = 0L))
  all    <- list()

  repeat {
    req <- httr2::request(url) |>
      httr2::req_headers(
        Authorization = paste("Bearer", token),
        Accept        = "application/json"
      )

    if (!is.null(params)) {
      req <- do.call(httr2::req_url_query, c(list(req), params))
    }

    resp   <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    txt    <- httr2::resp_body_string(resp)

    if (status >= 400) {
      stop(sprintf("fetch_profiles_api failed (%s): %s", status, txt))
    }

    payload <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    all     <- c(all, payload$results %||% list())

    url    <- payload$`next`
    params <- NULL  # `next` already has query params
    if (is.null(url) || !nzchar(url)) break
  }

  all
}

fetch_profile_api <- function(token = NULL, profile_id) {
  if (is.null(token) || !nzchar(token)) {
    token <- Sys.getenv("CSIAPPS_ACCESS_TOKEN")
  }
  if (!nzchar(token)) {
    stop("fetch_profile_api: no CSIAPPS_ACCESS_TOKEN set; user not authenticated?")
  }

  path <- sprintf("%s%s", PROFILE_ENDPOINT, profile_id)  # "/api/registration/profile/{id}"
  url  <- paste0(SITE_URL(), path)

  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      Accept        = "application/json"
    )

  resp   <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  txt    <- httr2::resp_body_string(resp)

  if (status >= 400) {
    stop(sprintf("fetch_profile_api failed (%s): %s", status, txt))
  }

  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# -------------------------------------------------------------------
# PKCE helpers
# -------------------------------------------------------------------

pkce_base64url <- function(raw_bytes) {
  b64 <- openssl::base64_encode(raw_bytes)
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  sub("=+$", "", b64)
}

#' Encode a PKCE code verifier into a state string for the auth request
#'
#' @param verifier a PKCE code verifier string
#' @importFrom stats runif
#'
pkce_state_encode <- function(verifier) {
  payload <- jsonlite::toJSON(
    list(
      v = verifier,
      r = as.integer(runif(1, 1, 1e9))
    ),
    auto_unbox = TRUE
  )
  pkce_base64url(charToRaw(payload))
}

pkce_base64url_decode <- function(x) {
  x <- gsub("-", "+", x, fixed = TRUE)
  x <- gsub("_", "/", x, fixed = TRUE)
  padding <- 4 - (nchar(x) %% 4)
  if (padding < 4) x <- paste0(x, strrep("=", padding))
  openssl::base64_decode(x)
}

pkce_state_decode <- function(state) {
  raw <- pkce_base64url_decode(state)
  jsonlite::fromJSON(rawToChar(raw))
}

# -------------------------------------------------------------------
# Token exchange
# -------------------------------------------------------------------

exchange_code_for_token <- function(code, code_verifier = NULL) {
  req <- httr2::request(CSIAPPS_TOKEN_URL()) |>
    httr2::req_auth_basic(Sys.getenv("CSIAPPS_CLIENT_ID"), Sys.getenv("CSIAPPS_CLIENT_SECRET")) |>
    httr2::req_body_form(
      grant_type    = "authorization_code",
      code          = code,
      redirect_uri  = Sys.getenv("CSIAPPS_REDIRECT_URI"),
      code_verifier = code_verifier
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)  # don't throw on HTTP errors

  resp     <- httr2::req_perform(req)
  status   <- httr2::resp_status(resp)
  body_txt <- httr2::resp_body_string(resp)
  body <- tryCatch(
    jsonlite::fromJSON(body_txt, simplifyVector = TRUE),
    error = function(e) list(raw = body_txt)
  )

  if (status >= 200 && status < 300) {
    body
  } else {
    list(
      error   = "token_exchange_http_error",
      status  = status,
      payload = body
    )
  }
}
