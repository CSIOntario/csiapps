# CSIAPPS REST API

``` r
library(csiapps)
library(dplyr)
```

`csiapps` includes a generic
[`make_request()`](https://csiontario.github.io/csiapps/reference/make_request.md)
function to make requests to the `CSIAPPS` REST API. Please refer to the
[CSIAPPS Swagger documentation](https://apps.csiontario.ca/api/swagger/)
for full details on available endpoints, parameters, and response
formats.

Below are some common use cases for interacting with the API endpoints
using `csiapps`.

### Data Warehouse Ingestion

To ingest raw data into the warehouse, we make a POST request to the
`/api/warehouse/ingestion/primary/` endpoint. The only mandatory query
parameter is the `source_uuid` of the target table, a list of `records`
to be ingested, the `subject_field` by which the record can be uniquely
identified. The records must comply with the schema defined for the
target table.

``` r
make_request(
  endpoint = "api/warehouse/ingestion/primary",
  method = "POST",
  body = list(
    source_uuid = Sys.getenv("SOURCE_UUID"),
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
    ) |>
  dplyr::bind_rows()
```

### Data Record Flattening

After retrieving records,
[`flatten_record()`](https://csiontario.github.io/csiapps/reference/flatten_record.md)
can be used to flatten nested records into a more tabular format for
easier analysis. It retains the original `data` payload and also
includes additional metadata fields such as the unique `id`, the
timestamp of when the record was collected, and any other relevant
information.

``` r
flat_records <- 
  records |> 
  dplyr::pull(results) |> 
  lapply(flatten_record)
```
