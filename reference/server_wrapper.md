# Wrapper server function for Shiny apps

Handles OAuth2 PKCE authentication flow with CSIAPPS, managing user
tokens and info, and providing a consistent authentication status UI.

## Usage

``` r
server_wrapper(app_specific_logic)
```

## Arguments

- app_specific_logic:

  Existing server logic of shiny web application

## Value

A Shiny server function that wraps the provided app-specific logic with
authentication handling and user info retrieval.
