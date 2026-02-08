
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csiapps

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `csiapps` from
[GitHub](https://github.com/CSIOntario/csiapps) with:

``` r
# install.packages("remotes")
remotes::install_github("CSIOntario/csiapps")
```

The following environment variables must be set to use the package:

- `CSIAPPS_CLIENT_ID`: The client ID for the application registered in
  CSIAPPS.
- `CSIAPPS_CLIENT_SECRET`: The client secret for the application
  registered in CSIAPPS.
- `CSIAPPS_AUTH_URL`: The URL for the authentication endpoint of the
  API.
- `CSIAPPS_TOKEN_URL`: The URL for the token endpoint of the API.
- `CSIAPPS_REDIRECT_URL`: The URL to which the API will redirect after
  authentication.
- `CSIAPPS_USERINFO_URL`: The URL for the user info endpoint of the API.
- `CSIAPPS_SCOPE`: The scope of the authentication request.

## Usage

``` r
# check_secrets()
# ui_wrapper
# token_wrapper
# should I just make a vignette?
```
