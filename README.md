
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csiapps <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `csiapps` from
[GitHub](https://github.com/CSIOntario/csiapps) with:

``` r
# install.packages("remotes")
remotes::install_github("CSIOntario/csiapps")
```

## Usage

The following environment variables must be set to use the package:

- `CSIAPPS_CLIENT_ID`: The client ID for the application registered in
  CSIAPPS.
- `CSIAPPS_CLIENT_SECRET`: The client secret for the application
  registered in CSIAPPS.
- `CSIAPPS_REDIRECT_URL`: The URL to which the application will redirect
  after authentication.
- `CSIAPPS_SCOPE`: (optional) The scope of the authentication request.

Please refer to `vignette("csiapps")` for more information on how to use
this library.
