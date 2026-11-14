# csiapps docs

Source for the cross-language documentation site covering both `csiapps`
packages — [R](https://github.com/CSIOntario/csiapps-r) and
[Python](https://github.com/CSIOntario/csiapps-py) — built with
[MkDocs Material](https://squidfunk.github.io/mkdocs-material/).

It covers the shared warehouse/sandbox API, Shiny, Dash, and Streamlit
application wrappers, and authenticated Quarto documents. Published behavior
changes are summarized in `docs/release-notes.md`; upgrade guidance is in
`docs/migrating.md`.

**Live site: https://csiontario.github.io/csiapps/**

The Python API reference (`docs/api.md`) is autodoc'd via `mkdocstrings` from
the `csiapps` package installed from `csiapps-py@main` (see `requirements.txt`).
The R reference lives with the R package (pkgdown, at `…/csiapps-r/`).

## Build locally

```bash
pip install -r requirements.txt
mkdocs build --strict # check links, reference generation, and documentation warnings
mkdocs serve          # preview at http://127.0.0.1:8000
```

## Deploy

Automatic: pushing to `main` runs `.github/workflows/docs.yml`, which builds
and publishes to the `gh-pages` branch.
