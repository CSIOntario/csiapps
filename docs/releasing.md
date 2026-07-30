# Releasing `csiapps`

How to ship a change to the `csiapps` **Python** package safely — the
canary→publish loop, from a local edit to a published release with every
dependent app redeployed.

!!! info "This page is for maintainers of the `csiapps` package"
    If you are *using* `csiapps` to build an app, you want the tutorials
    instead: [Shiny apps](shiny-apps.md), [Dash apps](dash-apps.md),
    [Sandbox mode](sandbox.md), and the [REST API](rest-api.md).

> The R package (`csiapps-r`) follows the same shape against its own dummy app
> and CRAN/pkgdown; this page covers the Python side.

## The canary principle

Production apps depend on `csiapps`, so an unforeseen change can silently break
them. We don't rely on catching that by eye — we use the **dummy apps** as
regression harnesses:

- [`dummy-python-shiny`](https://github.com/CSIOntario/dummy-python-shiny) — the
  Shiny canary.
- [`dummy-python-dash`](https://github.com/CSIOntario/dummy-python-dash) — the
  Dash canary.

Each exercises the whole `csiapps` public surface. A **clean render plus two
green self-test boards** (sandbox + live) is our evidence that a change is safe.
Every release runs the dummy apps twice: once **locally** against the working
copy, and once again on the **deployed** apps after the change is pushed.

## The loop

### 1. Make the change locally

Edit `csiapps-py` and run its own test suite:

```bash
cd csiapps-py
uv run pytest            # or: .venv/bin/python -m pytest
```

Both framework test groups skip themselves when their extra is absent; run the
full matrix if you touched shared code.

### 2. Validate against the dummy apps locally

Install the **working copy** into each dummy app (the editable install overrides
the pinned wheel) and run its gates. Do this for **both** frameworks — a change
to shared core code must be green in each:

```bash
# in each dummy app dir, with its own .venv active
pip install -e '../csiapps-py[shiny]'    # dummy-python-shiny
pip install -e '../csiapps-py[dash]'     # dummy-python-dash
```

There are three gates. The first is cheap and unconditional; the other two are
the same live suite reached two different ways.

**a) Sandbox gate** — fast, no credentials, no network. Must all PASS and exit 0:

```bash
.venv/bin/python selftest.py
```

**b) Live gate, in the app** — this is the one that normally matters. Run the
app, log in through the browser, and read the live self-test board:

```bash
.venv/bin/shiny run --reload app.py       # dummy-python-shiny  (:8000)
.venv/bin/python app.py                    # dummy-python-dash   (:8050 or the
                                           # port in CSIAPPS_REDIRECT_URI)
```

The app calls `load_dotenv()`, so it picks up `.env`. For the live board it
needs production mode plus the OAuth credentials — `CSIAPPS_ENV=production`,
`CSIAPPS_CLIENT_ID`, `CSIAPPS_CLIENT_SECRET`, `CSIAPPS_REDIRECT_URI`,
`CSIAPPS_SCOPE`, and additionally `CSIAPPS_SECRET_KEY` for Dash. It does
**not** need `CSIAPPS_ACCESS_TOKEN`: once you have logged in, the access token
comes from the web session. Set `CSIAPPS_TEST_SOURCE_UUID` as well, or the
ingest round-trip skips itself.

While you are there, eyeball the app: chrome renders, both boards green,
Registry + Warehouse tabs work.

**c) Live gate, from the CLI** — optional, and it has a sharp edge:

```bash
CSIAPPS_ENV=production CSIAPPS_ACCESS_TOKEN=<token> \
  .venv/bin/python selftest.py --live
```

`selftest.py` does **not** call `load_dotenv()` — only `app.py` does — so it
sees nothing but the ambient shell environment. And with no web session,
`current_token()` falls back to `CSIAPPS_ACCESS_TOKEN`, which is blank in the
checked-in `.env` files. Both values have to be exported in the shell.

!!! warning "A green `--live` run can mean zero live checks ran"
    The live suite returns early with a **SKIP** row if the app is in sandbox
    mode, and again if no token is available. `SKIP` is not `FAIL`, and
    `selftest.py` exits non-zero only on failures — so a run with every live
    check skipped still prints a tidy summary and exits 0. Always read the
    `N passed, N failed, N skipped` line rather than trusting the exit code.

Run each app in **sandbox** mode (the default) at minimum; run it in
**production** mode too if the change touches auth, the client, or chrome.

**Green everywhere ⇒ proceed. Any red ⇒ fix `csiapps` and repeat.**

### 3. Push `csiapps` to `main`

Once local is green, push the branch / merge to `main`. Nothing is published to
PyPI yet — this only makes the change available from source.

### 4. Re-validate on the *deployed* dummy apps

The point of this step is to catch what a local editable install can't: real
OAuth, real data, the deployment environment, multi-request behaviour. Point the
deployed dummy apps at the unreleased code and redeploy them:

- Temporarily install `csiapps` **from `main`** rather than the pinned release —
  in the dummy app's `requirements.txt`:

  ```text
  # pre-release validation only; revert to the pinned version before step 5
  csiapps[dash] @ git+https://github.com/CSIOntario/csiapps-py.git@main
  ```

- Redeploy the dummy app (Posit Connect) and open it. Confirm: login returns
  "Signed in as …", both self-test boards are green in production, Registry shows
  real orgs/athletes, and the Warehouse round-trip works.

**Green on the deployed dummy apps ⇒ the change is safe to make official.**

### 5. Make it official

1. **Bump the version** in `pyproject.toml` (and `__version__`), following
   semver — a breaking public-surface change is a major/minor bump, not a patch.
2. **Publish to PyPI.** There is no release workflow in the repo yet, so this is
   manual: tag the commit, build, and upload.

    ```bash
    git tag v<new version> && git push --tags
    uv build                     # or: python -m build
    uv publish                   # or: python -m twine upload dist/*
    ```

3. **Update the docs** — [`csiapps`](https://github.com/CSIOntario/csiapps), the
   repo behind this site: the [Python reference](api.md) autodocs from
   `csiapps-py@main`, but the tutorials, the
   [parity checklist](parity.md), and any changed behaviour are hand-written and
   must be edited. Pushing to `main` there redeploys the site.
4. **Bump the pin and redeploy consumers.** Change `csiapps[...]==<new version>`
   in the dummy apps (revert the step-4 git line) **and** in every dependent
   app, then redeploy. The dummy apps should be green one more time on the
   pinned release.

## Checklist

- [ ] `pytest` green in `csiapps-py`.
- [ ] `selftest.py` (sandbox) green in **both** dummy apps against the working copy.
- [ ] Each dummy app renders and both boards are green locally, in production mode.
- [ ] Any `selftest.py --live` run checked for skipped rows, not just exit 0.
- [ ] Change pushed to `csiapps-py@main`.
- [ ] Deployed dummy apps green in production (installed from `main`).
- [ ] Version bumped; published to PyPI.
- [ ] Docs updated (tutorials / parity / behaviour notes).
- [ ] `csiapps==<version>` pin bumped and redeployed in the dummy apps and every
      dependent app.

## Notes

- **Breaking changes** to the app-wrapper contract (route paths, required env
  vars, import locations) break already-deployed apps the moment they reinstall.
  Call them out in the release notes and in the consumer-app bump so nobody is
  surprised at redeploy time. If the change affects existing Python apps, add it
  to [Migrating Python apps](migrating.md).
- **Keep the dummy apps in lockstep.** If a change adds public surface, extend
  `selftest.py` in the dummy apps to cover it — the canary is only as good as
  what it exercises.
