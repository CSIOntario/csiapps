# Flatten a record object into a simpler structure

Flatten a record object into a simpler structure

## Usage

``` r
flatten_record(rec)
```

## Arguments

- rec:

  a data record object as returned by the warehouse API endpoints

## Value

a list with flattened fields for easier analysis, including `id`,
`dataset_uuid`, `profile` (subject name), `created_at`, `updated_at`,
`sport`, and `data` (original data payload)
