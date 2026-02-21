# Default configuration for Warehouse API client

Default configuration for Warehouse API client

## Usage

``` r
WarehouseAPIConfig(
  base_url = SITE_URL(),
  ingest_path = "/api/warehouse/ingestion/primary/",
  datasource_detail_pattern = "/api/warehouse/data-sources/%s/",
  records_list_path = "/api/warehouse/data-records/",
  timeout_s = 20L
)
```

## Arguments

- base_url:

  the base URL for the Warehouse API

- ingest_path:

  the path for the ingestion endpoint

- datasource_detail_pattern:

  a pattern for datasource detail endpoint

- records_list_path:

  the path for the data records endpoint

- timeout_s:

  timeout for API requests in seconds

## Value

A list containing the API configuration
