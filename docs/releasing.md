# Releasing `csiapps`

How to ship a change to `csiapps` safely — the canary→publish loop, from a local
edit to a published release with every dependent app redeployed.

`csiapps` is two packages, and both follow the same five-step loop. Pick your
language with the tabs; the choice sticks across the whole page.

!!! info "This page is for maintainers of the `csiapps` packages"
    If you are *using* `csiapps` to build an app, you want the tutorials
    instead: [Shiny apps](shiny-apps.md), [Dash apps](dash-apps.md),
    [Streamlit apps](streamlit-apps.md), [Sandbox mode](sandbox.md), and the
    [REST API](rest-api.md).

## The canary principle

Production apps depend on `csiapps`, so an unforeseen change can silently break
them. We don't rely on catching that by eye — we use the **dummy apps** as
regression harnesses:

=== "R"

    - [`dummy-r-shiny`](https://github.com/CSIOntario/dummy-r-shiny) — the Shiny
      canary, and the only one R needs (there is no R Dash package).
    - `dummy-r-quarto` — the authenticated R Quarto document canary.

=== "Python"

    - [`dummy-python-shiny`](https://github.com/CSIOntario/dummy-python-shiny) —
      the Shiny canary.
    - [`dummy-python-dash`](https://github.com/CSIOntario/dummy-python-dash) —
      the Dash canary.
    - [`dummy-python-streamlit`](https://github.com/CSIOntario/dummy-python-streamlit) —
      the native Streamlit canary for Posit Connect Cloud.
    - `dummy-python-quarto` — the authenticated Python Quarto document canary.

Together they exercise the relevant `csiapps` public surface. Every release
validates the dummy apps twice: first **locally**, where the sandbox self-test
runs against the working copy without credentials, and then on the **deployed**
app after the change is pushed. A clean deployed render plus two green self-test
boards (sandbox + live) is our evidence that a change is safe.

For a Quarto change, deploy both `dummy-r-quarto` and
`dummy-python-quarto` to **Connect Cloud** from `staging`. Connect Cloud is the
primary release gate: verify the anonymous login shell, two-viewer isolation,
protected-body delivery, logout, a 404 for the internal body file, both
self-test boards, authenticated Registry reads, and the Warehouse round-trip.
Then run a secondary smoke deployment on self-hosted Posit Connect. Snapshot
validation is deferred until that optional layer enters development. Only the
exact commit that passes both checks may advance from `staging` to `main` and
receive a version tag.

## The loop

### 1. Make the change locally

=== "R"

    Edit `csiapps-r` and run its own gates. `document()` first — `man/` and
    `NAMESPACE` are committed to the repo, so a roxygen change that isn't
    regenerated ships stale docs:

    ```r
    devtools::document()   # regenerate man/ + NAMESPACE
    devtools::test()       # testthat
    devtools::check()      # full R CMD check
    ```

    `devtools::test()` is the fast inner loop; `check()` is slower but catches
    the documentation, `NAMESPACE`, and example problems `test()` won't. Run
    `check()` at least once before you tag in step 5. The same gates run in CI
    on every push via `.github/workflows/R-CMD-check.yaml`.

=== "Python"

    Edit `csiapps-py` and run its own gates:

    ```bash
    uv run pytest
    uv run ruff check .
    uv build
    uvx twine check --strict dist/*
    ```

    Framework test groups skip themselves when their extra is absent; run the
    full Shiny, Dash, Streamlit, and Quarto matrix if you touched shared code.
    Test clean installs on the deployment Python version as well as an existing
    development environment: an editable install can retain older dependencies.
    Shiny CI covers 1.6.3 and the latest version on Python 3.11; Quarto also tests
    the latest version on Python 3.11. Shiny 1.9 runs asynchronous reactive effects
    concurrently, so authentication checks must not depend on effect order.

### 2. Validate against the dummy apps locally

=== "R"

    **There is nothing to install.** The dummy app's
    [`csiapps_dev.R`](https://github.com/CSIOntario/dummy-r-shiny/blob/main/csiapps_dev.R)
    calls `pkgload::load_all("../csiapps")`, so it picks up the sibling source
    checkout on every run — edit the package, run the app, see the result. Just
    make sure the two repos are siblings on disk (or point `CSIAPPS_SRC` at the
    source).

    !!! tip "Confirm which copy you are testing"
        The app's Environment panel and the `selftest.R` summary both name the
        loaded copy — e.g., `csiapps: 0.1.6 (local source: ../csiapps)` in dev, or
        `csiapps: 0.1.6 (staging @ 22f36ef)` when it fell back to the installed
        package. If you see a ref and commit where you expected local source,
        you are validating the wrong code. Set `CSIAPPS_SRC=""` deliberately
        when you *do* want to test the installed release.

    Run the sandbox gate. It is fast, uses no credentials or environment
    variables, and makes no network requests. All checks must PASS and exit 0:

    ```bash
    Rscript selftest.R
    ```

=== "Python"

    Install the **working copy** into each dummy app (the editable install
    overrides the pinned wheel) and run its gates. Do this for **all four**
    integrations — a change to shared core code must be green in each:

    ```bash
    # in each dummy app dir, with its own .venv active
    pip install -e '../csiapps-py[shiny]'    # dummy-python-shiny
    pip install -e '../csiapps-py[dash]'     # dummy-python-dash
    pip install -e '../csiapps-py[streamlit]' # dummy-python-streamlit
    pip install -e '../csiapps-py[quarto]'    # dummy-python-quarto
    ```

    Each app's Environment panel and sandbox summary should report
    `csiapps: <version> (local editable)`. On a deployed Git install they report
    the requested ref and exact resolved commit instead.

    Run the sandbox gate in each app. It is fast, uses no credentials or
    environment variables, and makes no network requests. All checks must PASS
    and exit 0:

    ```bash
    .venv/bin/python selftest.py
    .venv/bin/python -m unittest discover -s tests -v
    ```

The sandbox suite is the required credential-free local gate. You can also
validate the interactive app locally with real login and live API checks before
pushing; this supplements the deployed gate in step 4.

!!! info "Python: localhost does not determine authentication mode"
    `CSIAPPS_ENV=production` enables real CSI login and live requests, even on
    localhost. Any other value (or no value) selects sandbox, unless the app
    explicitly overrides the mode. The Shiny, Dash, and Streamlit dummy apps
    load `.env`; their example files select production mode. An exported value
    takes precedence over `.env`.

    For example, launch the Shiny dummy without credentials or live requests:

    ```bash
    CSIAPPS_ENV=sandbox CSIAPPS_ACCESS_TOKEN= .venv/bin/shiny run --reload --port 8000 app.py
    ```

    For optional local live validation, configure the OAuth client and its
    exact local redirect URL, then launch:

    ```bash
    CSIAPPS_ENV=production .venv/bin/shiny run --reload --port 8000 app.py
    ```

    After login, the live board uses the browser's session token. The sandbox
    board still uses local fixtures and must preserve that session. An active
    browser session never falls back to `CSIAPPS_ACCESS_TOKEN`; that fallback
    is for scripts outside a browser session. See each dummy README for its
    framework's launch command.

    The live suite includes an ingest round-trip when
    `CSIAPPS_TEST_SOURCE_UUID` identifies the dedicated test source; without
    it, that write check is skipped. In contrast, `python selftest.py` runs only
    sandbox checks and does not load `.env`. CLI `--live` requires production
    settings and a token exported into the process environment.

**Green everywhere ⇒ proceed. Any red ⇒ fix `csiapps` and repeat.**

### 3. Push the change

=== "R"

    Push the branch, then merge it into **`staging`** — the integration ref,
    and what the deployed dummy app validates:

    ```bash
    git push -u origin fix/my-change
    # open a PR against staging, review, merge
    ```

    Bump the version in `DESCRIPTION` on `staging` **now**, before step 4, not
    at release time. Then the commit the deployed dummy app validates is byte
    for byte the commit you tag in step 5, with no code moving in between.

    !!! danger "In R, a git ref *is* the distribution channel"
        There is no PyPI equivalent for `csiapps-r`. A bare
        `remotes::install_github("CSIOntario/csiapps-r")` resolves to `main`, so
        pushing there changes what everybody gets on their next reinstall,
        before you have validated anything deployed — and
        `.github/workflows/pkgdown.yaml` republishes the
        [reference site](https://csiontario.github.io/csiapps-r/) from `main`,
        documenting API that is not released yet. Merging to `main` is step 5.

        This does **not** reach already-deployed apps: their `manifest.json`
        pins a commit, so they stay put until someone regenerates it. The
        exposure is everyone's *next* install or deploy, plus the docs.

    See
    [which ref serves whom](https://github.com/CSIOntario/csiapps-r/blob/main/CONTRIBUTING.md#branches-which-ref-serves-whom)
    for the full table.

=== "Python"

    Push the branch, then merge it into **`staging`**, which is the integration
    ref consumed by the deployed dummy apps:

    ```bash
    git push -u origin fix/my-change
    # open a PR against staging, review, merge
    ```

    Bump `pyproject.toml` and `src/csiapps/__init__.py` on `staging` before the
    deployed gate. The exact commit that passes step 4 must be the commit later
    fast-forwarded to `main`, tagged, and published to PyPI.

    Production apps remain pinned to published PyPI versions, so merging an
    unreleased change to `staging` cannot move them. Keeping `main`
    release-aligned also prevents the Python API reference from documenting
    unreleased code, because this site installs `csiapps-py@main` when it builds.

### 4. Re-validate on the *deployed* dummy apps

This step verifies real OAuth, real data, and multi-request behaviour in the
deployment environment against the exact candidate commit. Point the
deployed dummy apps at the unreleased code and redeploy them.

=== "R"

    The deployed app does *not* use `../csiapps`. It is published by **Posit
    Connect Cloud**, which is git-backed: it reads `manifest.json` from the app
    repo and installs the exact `csiapps` commit pinned there as `GithubSHA1`.
    **Committing and pushing that manifest is the deployment** — there is no
    rsconnect push step.

    [`deploy.R`](https://github.com/CSIOntario/dummy-r-shiny/blob/main/deploy.R)
    installs `csiapps` from a git ref, verifies the install really is that
    commit, and regenerates the manifest. It defaults to `staging`:

    ```bash
    Rscript deploy.R
    git add manifest.json && git commit -m "Pin csiapps to staging" && git push
    ```

    Connect Cloud republishes automatically on push to the tracked branch. The
    pinned commit is visible in the diff as `GithubSHA1`, so a code review can
    confirm exactly what was deployed.

    !!! warning "Confirm the deployed app is actually running your commit"
        The build happens server-side, so nothing in your terminal proves it
        picked up the pin — and every commit on a branch reports the same
        `Version`. The app's **Environment panel prints the ref and commit**
        (e.g., `csiapps: 0.1.6 (staging @ 22f36ef)`); check it matches before you
        trust anything else on the page. `Rscript selftest.R` prints the same
        line. On Connect Cloud, the app reads this provenance from
        `manifest.json` when the installed package metadata omits it.

    !!! note "Nothing to edit by hand"
        You never hand-edit a dependency file. The manifest is generated, so
        step 5 simply regenerates it from the release tag instead of `staging`.

    Regenerate the manifest whenever **any** app file changes, not just when
    `csiapps` moves — it records a checksum per file.

    !!! info "Deployment settings are configured once, on the content"
        The seven environment variables from `.Renviron.example` live in the
        Connect Cloud content's **Variables** panel — nothing carries over from
        a local `.Renviron`, which is gitignored and not in the repo.
        `CSIAPPS_REDIRECT_URI` must be the *deployed* URL and registered as an
        OAuth redirect in CSIAPPS. If login fails on the deployed app but works
        locally, suspect these before suspecting your change.

=== "Python"

    Keep each applicable dummy app tracking **`staging`**. After the package's
    staging CI passes, its dispatch job sends the exact validated SHA to all
    four canaries. Each canary workflow tests that SHA, updates its
    `requirements.txt` and `manifest.json`, commits the pin, and mirrors the
    deployment repository. Connect Cloud then rebuilds from the tracked branch.
    The dependency is an immutable staging commit, for example:

    ```text
    csiapps[shiny] @ git+https://github.com/CSIOntario/csiapps-py.git@<staging-sha>
    csiapps[dash] @ git+https://github.com/CSIOntario/csiapps-py.git@<staging-sha>
    csiapps[streamlit] @ git+https://github.com/CSIOntario/csiapps-py.git@<staging-sha>
    csiapps[quarto] @ git+https://github.com/CSIOntario/csiapps-py.git@<staging-sha>
    ```

    The package repository needs `CSIAPPS_CANARY_TOKEN` to dispatch the canary
    workflows. Canary repositories use `SYNC_PAT` to push their deployment
    mirrors; it must have access to those destination repositories and permission
    to update workflow files when those files are included in a mirrored change.
    Check both the pin/test job and the mirror job before reviewing the apps.

    Regenerate manifests whenever app files change: they include file checksums.
    The automated pin workflow handles dependency updates. For a manual native
    Streamlit manifest, use `app.py` as the primary file:

    ```bash
    .venv/bin/rsconnect write-manifest streamlit --overwrite \
      --entrypoint app.py .
    ```

    !!! warning "Confirm the deployed package commit"
        Each app's Environment panel must show the resolved commit matching
        `csiapps-py@staging`. The requested ref may itself be the full SHA,
        because the workflow pins an immutable commit. A version alone is not
        enough because multiple staging commits can carry the same version.

    !!! info "Host access setting"
        Enable **Public Access** on each canary's Connect Cloud content. This
        lets `csiapps` redirect visitors to CSI Access Portal.
        A host login in front of the app does not validate CSI authentication;
        the app itself remains protected by its framework wrapper.

Open the deployed app and confirm: login returns "Signed in as …", both
self-test boards are green in production, Registry shows real orgs/athletes, and
the Warehouse round-trip works.

**Green on the deployed dummy apps ⇒ the change is safe to make official.**

### 5. Make it official

=== "R"

    1. **Fast-forward `staging` into `main`.** *This is publication.* From this
       moment `remotes::install_github("CSIOntario/csiapps-r")` serves the new
       code, and `.github/workflows/pkgdown.yaml` rebuilds the R reference site.

        ```bash
        git checkout main && git merge --ff-only staging && git push
        ```

        `--ff-only` is not a style preference. A merge commit would put the tag
        on a commit that never went through step 4; a fast-forward means the
        commit you validated on the deployed app is the commit you tag. If it
        refuses, `main` has moved — rebase `staging` onto it and re-run step 4,
        because the combination is then untested.

        The version was already bumped in step 3, so nothing changes here.

    2. **Tag and cut a GitHub Release** so consumers have a stable ref to pin.
       The release body is where the notes for this version live — write them
       there, since the package carries no changelog file:

        ```bash
        git tag -a v<new version> -m "csiapps <new version>"
        git push --tags
        gh release create v<new version> --generate-notes
        ```

        `--generate-notes` seeds the body from the merged commits; edit it to
        call out anything that changes behaviour for app authors.

    3. **Update the docs** —
       [`csiapps`](https://github.com/CSIOntario/csiapps), the repo behind this
       site. The [R reference](https://csiontario.github.io/csiapps-r/reference/)
       is pkgdown and rebuilds itself from `main`, but the tutorials on this
       site, the [parity checklist](parity.md), and any changed behaviour are
       hand-written and must be edited. Pushing to `main` there redeploys this
       site.
    4. **Re-pin and redeploy consumers.** Regenerate each app's manifest from
       the release tag rather than `staging`:

        ```bash
        Rscript deploy.R --ref=v<new version>
        git add manifest.json && git commit -m "Pin csiapps to v<new version>" && git push
        ```

        Do the same in every dependent R app — production apps pin **tags**,
        never `staging` or `main`. The dummy app should be green one more time
        on the pinned tag, with its Environment panel showing the tag.

=== "Python"

    1. **Fast-forward `staging` into `main`.** If this fails, `main` moved and
       the combined commit has not passed the deployed gate; reconcile the
       branches and repeat step 4.

        ```bash
        git checkout main && git merge --ff-only staging && git push
        ```

       The version was already bumped in step 3, so no source file changes here.

    2. **Publish the validated commit to PyPI.** There is no release workflow in
       the repo yet. Tag the exact validated SHA and upload only its checked
       wheel and source distribution. Prefer the `dist` artifact from that
       commit's successful CI build; otherwise build from a clean checkout of
       the tag. Do not upload an old `dist/*` directory containing other versions.

        ```bash
        git tag -a v<new version> <validated-sha> -m "csiapps <new version>"
        git push origin v<new version>
        uv build --out-dir dist/<new version>
        uvx twine check --strict dist/<new version>/*
        uvx twine upload --repository pypi dist/<new version>/*
        ```

    3. **Update the docs** —
       [`csiapps`](https://github.com/CSIOntario/csiapps), the repo behind this
       site: the [Python reference](api.md) autodocs from `csiapps-py@main`, but
       the tutorials, the [parity checklist](parity.md), and any changed
       behaviour are hand-written and must be edited. Pushing to `main` there
       redeploys the site.
    4. **Validate the published artifact and redeploy production consumers.**
       Install `csiapps[shiny]==<new version>`, `csiapps[dash]==<new version>`,
       `csiapps[streamlit]==<new version>`, and `csiapps[quarto]==<new version>`
       in separate clean temporary environments. Run each corresponding dummy
       app's sandbox self-test and regression tests. Confirm the installed copy
       comes from PyPI, not an editable checkout. Create a GitHub Release with
       notes explaining behavior changes and migration requirements.

       Update each production app deliberately, after checking its imports and
       compatibility with the new version. Keep the dummy apps' committed
       requirements on their exact staging SHA; they continue tracking future
       staging changes through automation. If the shared Quarto extension also
       changed, promote its validated staging commit to `main` so
       `quarto add CSIOntario/csiapps-quarto` obtains the updated browser bridge.

## Checklist

=== "R"

    - [ ] `devtools::document()` run; `man/` and `NAMESPACE` changes committed.
    - [ ] `devtools::test()` and `devtools::check()` green in `csiapps-r`.
    - [ ] `Rscript selftest.R` (sandbox) green in `dummy-r-shiny` against the
          working copy — with the `pkgload::load_all` line confirmed in the log.
    - [ ] Change merged into **`staging`** on `csiapps-r` (not `main`).
    - [ ] `DESCRIPTION` version bumped **on `staging`, before** the deployed
          gate — so the validated commit is the tagged commit.
    - [ ] `Rscript deploy.R` run in `dummy-r-shiny`; regenerated
          `manifest.json` committed and pushed.
    - [ ] Deployed dummy app green in production, **and its Environment panel
          shows the commit that was just pinned**.
    - [ ] `staging` fast-forwarded into `main` (`--ff-only`); tag pushed and
          GitHub Release created, with notes covering anything that changes
          behaviour for app authors.
    - [ ] Docs updated (tutorials / parity / behaviour notes).
    - [ ] `deploy.R --ref=v<version>` re-run in the dummy app and every
          dependent R app; manifests committed and pushed.

=== "Python"

    - [ ] `pytest` green in `csiapps-py`.
    - [ ] `selftest.py` (sandbox) and `python -m unittest discover -s tests -v`
          green in **all four** Python dummy apps against the working copy.
    - [ ] Change merged into **`csiapps-py@staging`**, not `main`.
    - [ ] Version bumped in both files before the deployed gate.
    - [ ] All four canary workflows green, including deployment mirrors;
          requirements and manifests pin the exact candidate staging SHA.
    - [ ] Deployed dummy apps green in production and showing the exact staging
          commit in their Environment panel.
    - [ ] `staging` fast-forwarded into `main`; validated commit tagged, built,
          checked, and published to PyPI.
    - [ ] Docs updated (tutorials / parity / behaviour notes).
    - [ ] Published `csiapps==<version>` artifact validated in a clean
          environment for all four extras; production-app migrations reviewed
          individually. Dummy apps retain their exact staging SHA pins.

## How the two release processes differ

Everything above follows the same five steps, but the mechanics diverge in ways
worth knowing before you run the loop in the other language:

| | R | Python |
|---|---|---|
| Canaries | `dummy-r-shiny` + `dummy-r-quarto` | `dummy-python-shiny` + `dummy-python-dash` + `dummy-python-streamlit` + `dummy-python-quarto` |
| Test command | `devtools::test()` / `devtools::check()` | `uv run pytest` |
| Regenerate docs from source | `devtools::document()` commits `man/` + `NAMESPACE` | not needed — mkdocstrings reads the source |
| Getting the working copy into the dummy app | automatic, `pkgload::load_all("../csiapps")` | `pip install -e '../csiapps-py[extra]'` |
| Env vars for the CLI self-test | `.Renviron`, auto-loaded | must be exported in the shell |
| Integration branch | **`staging`** | **`staging`** |
| Deploy pin | generated `manifest.json`, pins an exact `GithubSHA1` | automated exact staging SHA in `requirements.txt` and regenerated manifest |
| Pre-release deploy | `deploy.R` (defaults to `staging`); nothing to revert | successful staging CI dispatches all four canaries; nothing to revert |
| Publishing the deploy | `git push` the manifest; Connect Cloud rebuilds | pin workflow pushes and mirrors; Connect Cloud rebuilds |
| Distribution | GitHub — no registry | PyPI |
| Effect of merging to `main` | **publishes** to every `install_github()` consumer, and to the pkgdown site | updates source/API docs; package consumers wait for `uv publish` |
| Version file | `DESCRIPTION` | `pyproject.toml` + `__version__` |
| Version bumped at | step 3, on `staging` | step 3, on `staging` |
| Release notes | GitHub Release body | GitHub Release body and migration documentation |
| Reference docs | separate [pkgdown site](https://csiontario.github.io/csiapps-r/), auto-built from `main` | [mkdocstrings](api.md), inside this site |

!!! question "Why does Python use `staging` when PyPI is already a gate?"
    PyPI protects production package consumers, but it does not provide a
    deployed canary. The Python `staging` branch lets Posit Connect exercise an
    unreleased commit while production apps remain pinned to PyPI. It also
    keeps `main` aligned with releases, which matters because the official
    Python API reference is built from `csiapps-py@main`. The dummy apps remain
    on `staging` after a release so the next staged change reaches them
    automatically; they never follow `main` or a PyPI pin.

## Notes

- **Breaking changes** to the app-wrapper contract (route paths, required env
  vars, import locations) break already-deployed apps the moment they reinstall.
  Call them out in the release notes and in the consumer-app bump so nobody is
  surprised at redeploy time. If the change affects existing Python apps, add it
  to [Migrating Python apps](migrating.md).
- **Keep the dummy apps in lockstep.** If a change adds public surface, extend
  the self-test in the dummy apps to cover it — the canary is only as good as
  what it exercises.
- **A change to only one language is still a docs change.** This site documents
  both packages side by side, so a new function in one language either needs a
  matching implementation in the other or a new row in the
  [parity checklist](parity.md) explaining the divergence.
