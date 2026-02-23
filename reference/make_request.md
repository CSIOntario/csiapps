# Make an authenticated API request to CSIAPPS

Make an authenticated API request to CSIAPPS

## Usage

``` r
make_request(
  endpoint,
  method = "GET",
  body = NULL,
  query = list(),
  base_url = SITE_URL(),
  headers = list(),
  token = Sys.getenv("CSIAPPS_ACCESS_TOKEN"),
  timeout = 20L,
  verbose = FALSE,
  paginate = FALSE,
  max_pages = 50
)
```

## Arguments

- endpoint:

  API endpoint path.

- method:

  HTTP method. Defaults to "GET"

- body:

  Optional request body for POST/PUT/PATCH requests; should be an R
  object that can be serialized to JSON

- query:

  Optional list of query parameters to include in the request URL

- base_url:

  Base URL for the API; defaults to SITE_URL()

- headers:

  Optional list of additional HTTP headers to include in the request

- token:

  Authentication token. Will attempt to read from CSIAPPS_ACCESS_TOKEN
  environment variable if not provided explicitly.

- timeout:

  Request timeout in seconds; defaults to 20

- verbose:

  If TRUE, prints request and response details to the console for
  debugging purposes

- paginate:

  If TRUE, will attempt to paginate through results using "next" links
  in the API response. Defaults to FALSE.

- max_pages:

  Maximum number of pages to fetch when paginate = TRUE; defaults to 50
  to prevent infinite loops

## Value

List of parsed API responses
