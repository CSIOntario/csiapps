# CSIAPPS REST API

``` r
library(csiapps)
```

### Preamble

The following environment variables must be set to make requests to the
`CSIAPPS` REST API:

- `CSIAPPS_ACCESS_TOKEN`: The access token for authenticating API
  requests. This token should have the necessary permissions to access
  the desired endpoints.

### Overview

`csiapps` includes a generic
[`make_request()`](https://csiontario.github.io/csiapps/reference/make_request.md)
function to make requests to the `CSIAPPS` REST API. Please refer to the
[CSIAPPS Swagger documentation](https://apps.csiontario.ca/api/swagger/)
for full details on available endpoints, parameters, and response
formats.

Below are some common use cases for interacting with the API endpoints
using `csiapps`.

### Authorization Status

The most basic endpoint is the `/api/csiauth/me/` endpoint, which can be
used to check the status of the user making the request. This endpoint
requires authentication, so if the request is successful, it indicates
that your access token is valid and has the necessary permissions to
access the REST API.

``` r
result <- make_request(
  endpoint = "api/csiauth/me/"
)
```

### Data Warehouse Ingestion

To ingest raw data into the warehouse, we make a POST request to the
`/api/warehouse/ingestion/primary/` endpoint. Users supply the `source`
(uuid) of the target table, a list of `records` to be ingested, and
(optionally) the `subject_field` by which the record can be uniquely
identified. The records must comply with the schema defined for the
target table.

``` r
result <- make_request(
  endpoint = "api/warehouse/ingestion/primary/",
  method = "POST",
  body = list(
    source = Sys.getenv("SOURCE_UUID"),
    records = list(
      list(id = "xxxx", ...),
      list(id = "yyyy", ...),
      ...
    ),
    subject_field = "id"
  )
)
```

### Data Record Retrieval

To retrieve data records from the warehouse, we make an API request to
the `/api/warehouse/data-records/` endpoint. The only required parameter
is the `source_uuid` of the requested table, but there are several
optional parameters for additional filtering. Note that we can set the
`paginate` argument to `TRUE` to retrieve all records matching the query
parameters.

``` r
records <- make_request(
    endpoint = "api/warehouse/data-records",
    query = list(source_uuid = Sys.getenv("SOURCE_UUID")),
    paginate = T
    )
```
