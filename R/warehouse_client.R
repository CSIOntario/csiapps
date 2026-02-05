get_token <- function() {
  tok <- Sys.getenv("CSI_ACCESS_TOKEN")
  if (!nzchar(tok)) {
    stop("No CSI_ACCESS_TOKEN set; user may not be authenticated.", call. = FALSE)
  }
  tok
}

WarehouseClientError <- function(msg) {
  structure(list(message = msg), class = c("WarehouseClientError", "error", "condition"))
}

WarehouseAPIConfig <- function(
    institute = "csipacific",
    ingest_path = "/api/warehouse/ingestion/primary/",
    datasource_detail_pattern = "/api/warehouse/data-sources/%s/",
    records_list_path = "/api/warehouse/data-records/",
    timeout_s = 20L
) {
  list(
    base_url = paste0("https://apps.", institute, ".ca"),
    ingest_path = ingest_path,
    datasource_detail_pattern = datasource_detail_pattern,
    records_list_path = records_list_path,
    timeout_s = timeout_s
  )
}

new_warehouse_client <- function(config = WarehouseAPIConfig()) {

  get_auth_header <- function() {
    token <- get_token()
    paste("Bearer", token)
  }

  # GET JSON helper with logging
  GET_json <- function(url, params = NULL, verbose = FALSE) {
    req <- httr2::request(url) |>
      httr2::req_headers(
        Authorization = get_auth_header(),
        Accept        = "application/json"
      )

    if (!is.null(params) && length(params) > 0) {
      req <- do.call(httr2::req_url_query, c(list(req), params))
    }

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    txt    <- httr2::resp_body_string(resp)

    if (verbose) {
      cat("Warehouse GET", url, "status:", status, "\n")
      if (!is.null(params)) cat("  params:", paste(names(params), params, collapse = ", "), "\n")
      cat("  body:", txt, "\n")
    }

    if (status >= 400) {
      stop(WarehouseClientError(paste("GET failed with status", status, ":", txt)))
    }

    out <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = FALSE),
      error = function(e) {
        stop(WarehouseClientError(paste("Response not valid JSON:", e$message)))
      }
    )

    out
  }

  ingest_raw <- function(source_uuid,
                         records,
                         subject_field = NULL,
                         validate_client_side = FALSE,
                         verbose = FALSE) {

    if (length(records) == 0) {
      stop(WarehouseClientError("No records provided."))
    }

    payload <- list(
      source  = source_uuid,
      records = records
    )
    if (!is.null(subject_field)) {
      payload$subject_field <- subject_field
    }

    url <- paste0(config$base_url, config$ingest_path)

    req <- httr2::request(url) |>
      httr2::req_headers(
        Authorization = get_auth_header(),
        Accept        = "application/json",
        "Content-Type" = "application/json"
      ) |>
      httr2::req_body_json(payload) |>
      httr2::req_timeout(config$timeout_s)

    resp   <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    txt    <- httr2::resp_body_string(resp)


    if(verbose) {
      cat("Warehouse ingest POST", url, "status:", status, "\n")
      cat("  payload:", jsonlite::toJSON(payload, auto_unbox = TRUE), "\n")
      cat("  response:", txt, "\n")
    }

    if (status == 201L) {
      body <- tryCatch(
        jsonlite::fromJSON(txt, simplifyVector = TRUE),
        error = function(e) {
          stop(WarehouseClientError("201 response but body not valid JSON: " %+% e$message))
        }
      )
      dataset <- body$dataset
      created <- as.integer(body$created_records %||% 0L)
      if (is.null(dataset) || is.null(dataset$uuid)) {
        stop(WarehouseClientError("Ingest succeeded but dataset info missing."))
      }
      return(list(dataset = dataset, created = created))
    }

    # Non-201: surface server error
    stop(WarehouseClientError(paste("Ingest failed:", txt)))
  }

  list_records <- function(source_uuid,
                           collected_after = NULL,
                           collected_before = NULL,
                           role = NULL,
                           subject = NULL,
                           page_size = NULL,
                           extra_params = list(),
                           verbose = FALSE) {

    params <- c(
      list(source_uuid = source_uuid),
      extra_params
    )

    if (!is.null(collected_after))  params$collected_after  <- as.character(collected_after)
    if (!is.null(collected_before)) params$collected_before <- as.character(collected_before)
    if (!is.null(role))             params$role             <- role
    if (!is.null(subject))          params$subject          <- as.integer(subject)
    if (!is.null(page_size))        params$limit            <- as.integer(page_size)

    url <- paste0(config$base_url, config$records_list_path)
    results <- list()

    repeat {
      payload <- GET_json(url, params, verbose = verbose)

      # After first call, next URL already includes query; don't resend params
      params <- NULL

      if (is.list(payload) && !is.null(payload$results)) {
        items <- payload$results %||% list()
        results <- c(results, items)
        url <- payload$`next`
        if (is.null(url) || !nzchar(url)) break
      } else if (is.list(payload)) {
        # Non-paginated list
        results <- c(results, payload)
        break
      } else {
        stop(WarehouseClientError("Unexpected response from records endpoint."))
      }
    }

    results
  }

  list(
    config       = config,
    ingest_raw   = ingest_raw,
    list_records = list_records
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
`%+%`  <- function(a, b) paste0(a, b)
