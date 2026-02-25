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

To ingest data records into the warehouse, we make a POST request to the
`/api/warehouse/ingestion/primary/` endpoint. Users supply the `source`
(uuid) of the target *data source*, a list of `records` to be ingested,
and (optionally) the `subject_field` by which each record can be
uniquely assigned to a user. The records must comply with the schema
defined for the target data source.

#### JSON Schema Definition

Consider the following [JSON Schema](https://json-schema.org/learn):

``` json
{
  "title": "A registration form",
  "description": "A simple form example.",
  "type": "object",
  "required": [
    "id",
    "firstName",
    "lastName"
  ],
  "properties": {
    "id": {
      "type": "string",
      "title": "ID"
    },
    "firstName": {
      "type": "string",
      "title": "First name",
      "default": "Chuck"
    },
    "lastName": {
      "type": "string",
      "title": "Last name",
      "default": "Norris"
    },
    "age": {
      "type": "integer",
      "title": "Age"
    },
    "telephone": {
      "type": "string",
      "title": "Telephone",
      "minLength": 10
    }
  }
}
```

Here, `id` is the required field that serves as the unique user
assignment for each record.

To create records that comply with this schema, we create the following
records:

``` r
records <- list(
  list(id = "xxxx", firstName = "John", lastName = "Doe", age = 30, telephone = "1234567890"),
  list(id = "yyyy", firstName = "Jane", lastName = "Smith", age = 25, telephone = "0987654321")
)
```

To retrieve the JSON schema definition from the data warehouse, we can
make a request to the `/api/warehouse/data-sources/{uuid}/` endpoint,
where `uuid` is the uuid of the target data source. The schema
definition is located in the `head_primary_definition$schema` field of
the response.

``` r
data_source <- make_request(
  endpoint = paste0("api/warehouse/data-sources/", Sys.getenv("SOURCE_UUID"))
)

schema <- data_source$head_primary_definition$schema
```

#### Data Record Validation

We can validate data records using the `jsonlite` and `jsonvalidate`
packages. Be sure to set `auto_unbox = TRUE` when converting the JSON
objects to strings.

``` r
json_schema <- jsonvalidate::json_schema$new(jsonlite::toJSON(schema, auto_unbox = T))

validate_record <- function(record) {
  json_schema$validate(jsonlite::toJSON(record, auto_unbox = T))
}

validation_results <- sapply(records, validate_record)

stopifnot(all(validation_results))
```

Finally, we can make the API request to ingest these records into the
warehouse:

``` r
result <- make_request(
  endpoint = "api/warehouse/ingestion/primary/",
  method = "POST",
  body = list(
    source = Sys.getenv("SOURCE_UUID"),
    records = records,
    subject_field = "id"
  )
)
```

### Data Record Retrieval

To retrieve data records from the warehouse, we make an API request to
the `/api/warehouse/data-records/` endpoint. The only required parameter
is the `source_uuid` of the requested table, but there are several
optional parameters for additional filtering. Note that we can set the
`paginate = TRUE` to retrieve all records matching the query parameters.

``` r
records <- make_request(
    endpoint = "api/warehouse/data-records",
    query = list(source_uuid = Sys.getenv("SOURCE_UUID")),
    paginate = TRUE
    )
```
