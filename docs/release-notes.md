# Release notes

## Python 0.5.1

Released November 14, 2026. Available on
[PyPI](https://pypi.org/project/csiapps/0.5.1/) and as
[GitHub release v0.5.1](https://github.com/CSIOntario/csiapps-py/releases/tag/v0.5.1).
This release brings the applicable R 0.2.1 authentication and request
improvements to Python's Shiny, Dash, Streamlit, and Quarto integrations.

### Authentication

- Production browsers use their own session token. `CSIAPPS_ACCESS_TOKEN`
  remains available to scripts and optional sandbox seeding.
- HTTP 401, and HTTP 403 whose JSON `detail` is exactly
  `Authentication credentials were not provided.` or `Invalid token.`, clear
  only the matching rejected session and offer sign-in recovery. Ordinary
  permission and active-profile 403 responses preserve the session.
- Logout and failed authentication offer explicit sign-in/retry. Protected
  content is removed on expiry. Application caches must still be scoped to the
  viewer and cleared when that identity is lost.
- Shiny and Quarto support Shiny 1.9's concurrent asynchronous effects. Login
  publishes its token and successful state together; a late token exchange or
  user-info response cannot restore a signed-out session.
- Registered public PKCE clients may omit `CSIAPPS_CLIENT_SECRET`. Confidential
  clients keep their configured secret. Dash and Streamlit still require the
  separate `CSIAPPS_SECRET_KEY`.

### Requests and pagination

- List-valued query arguments produce repeated filters, for example
  `query={"subject": [1, 2]}`.
- Pagination follows `next` links or count/offset responses. Incomplete results,
  repeated page URLs, and premature empty pages raise an error. Callers that
  accepted truncated lists must handle the error or increase `max_pages`.
- Keyword-only `retry_max_tries` and `retry_is_transient` control retries.
  Authentication failures are not retried. Use `retry_max_tries=1` for a write
  that must not be repeated automatically.

See [request controls](rest-api.md#python-request-controls-051) for examples.

### Upgrading apps

Pin the extra matching your framework in its dependency file, for example:

```text
csiapps[shiny]==0.5.1
```

Use `[dash]`, `[streamlit]`, or `[quarto]` for those integrations; scripts that
need no web framework can use `csiapps==0.5.1`. Existing framework imports stay
the same for apps already on 0.3.0 or later. Apps on older versions also need
[the framework install/import migration](migrating.md).

For Quarto, refresh the shared extension with
`quarto add CSIOntario/csiapps-quarto` and re-render the document. Its browser
bridge removes OAuth `code` and `state` while preserving unrelated query
parameters.

Localhost apps can use production login when configured with
`CSIAPPS_ENV=production`; location does not select sandbox mode. The dummy apps'
`python selftest.py` command remains the credential-free sandbox gate, and the
in-app sandbox board preserves the viewer's login state. See
[sandbox and local test behavior](sandbox.md#local-apps-and-dummy-test-suites).

Review the [0.5.1 migration guidance](migrating.md#updating-to-python-051) and
validate each production app before redeploying it. Dummy apps keep tracking
exact staging commits for future release validation.
