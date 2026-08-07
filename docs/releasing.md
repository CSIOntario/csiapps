# Releasing `csiapps`

How to ship a change to `csiapps` safely — the canary→publish loop, from a local
edit to a published release with every dependent app redeployed.

`csiapps` is two packages, and both follow the same five-step loop. Pick your
language with the tabs; the choice sticks across the whole page.

!!! info "This page is for maintainers of the `csiapps` packages"
    If you are *using* `csiapps` to build an app, you want the tutorials
    instead: [Shiny apps](shiny-apps.md), [Dash apps](dash-apps.md),
    [Sandbox mode](sandbox.md), and the [REST API](rest-api.md).

## The canary principle

Production apps depend on `csiapps`, so an unforeseen change can silently break
them. We don't rely on catching that by eye — we use the **dummy apps** as
regression harnesses:

=== "R"

    - [`dummy-r-shiny`](https://github.com/CSIOntario/dummy-r-shiny) — the Shiny
      canary, and the only one R needs (there is no R Dash package).

=== "Python"

    - [`dummy-python-shiny`](https://github.com/CSIOntario/dummy-python-shiny) —
      the Shiny canary.
    - [`dummy-python-dash`](https://github.com/CSIOntario/dummy-python-dash) —
      the Dash canary.

Each exercises the whole `csiapps` public surface. A **clean render plus two
green self-test boards** (sandbox + live) is our evidence that a change is safe.
Every release runs the dummy apps twice: once **locally** against the working
copy, and once again on the **deployed** app after the change is pushed.

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

    Edit `csiapps-py` and run its own test suite:

    ```bash
    cd csiapps-py
    uv run pytest            # or: .venv/bin/python -m pytest
    ```

    Both framework test groups skip themselves when their extra is absent; run
    the full matrix if you touched shared code.

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

    There are three gates. The first is cheap and unconditional; the other two
    are the same live suite reached two different ways.

    **a) Sandbox gate** — fast, no credentials, no network. Must all PASS and
    exit 0:

    ```bash
    Rscript selftest.R
    ```

    **b) Live gate, in the app** — this is the one that normally matters. Run
    the app, log in through the browser, and read the live self-test board:

    ```bash
    Rscript -e 'shiny::runApp(port = 8000)'
    ```

    R auto-loads `.Renviron` from the project directory at startup, so the
    variables documented in `.Renviron.example` are picked up with no extra
    step. For the live board you need `CSIAPPS_ENV=production` plus
    `CSIAPPS_CLIENT_ID`, `CSIAPPS_CLIENT_SECRET`, `CSIAPPS_REDIRECT_URI`, and
    optionally `CSIAPPS_SCOPE`. You do **not** need `CSIAPPS_ACCESS_TOKEN` —
    once you have logged in, the token comes from the Shiny session. Set
    `CSIAPPS_TEST_SOURCE_UUID` as well, or the ingest round-trip skips itself.

    While you are there, eyeball the app: chrome renders, both boards green,
    Registry + Warehouse tabs work.

    **c) Live gate, from the CLI** — optional, no browser:

    ```bash
    Rscript selftest.R --live
    ```

    Because `.Renviron` is loaded for `Rscript` too, this needs no exported
    shell variables — but it does need a real `CSIAPPS_ACCESS_TOKEN` in there,
    since there is no web session to take one from.

=== "Python"

    Install the **working copy** into each dummy app (the editable install
    overrides the pinned wheel) and run its gates. Do this for **both**
    frameworks — a change to shared core code must be green in each:

    ```bash
    # in each dummy app dir, with its own .venv active
    pip install -e '../csiapps-py[shiny]'    # dummy-python-shiny
    pip install -e '../csiapps-py[dash]'     # dummy-python-dash
    ```

    There are three gates. The first is cheap and unconditional; the other two
    are the same live suite reached two different ways.

    **a) Sandbox gate** — fast, no credentials, no network. Must all PASS and
    exit 0:

    ```bash
    .venv/bin/python selftest.py
    ```

    **b) Live gate, in the app** — this is the one that normally matters. Run
    the app, log in through the browser, and read the live self-test board:

    ```bash
    .venv/bin/shiny run --reload app.py       # dummy-python-shiny  (:8000)
    .venv/bin/python app.py                   # dummy-python-dash   (:8050 or the
                                              # port in CSIAPPS_REDIRECT_URI)
    ```

    The app calls `load_dotenv()`, so it picks up `.env`. For the live board it
    needs production mode plus the OAuth credentials — `CSIAPPS_ENV=production`,
    `CSIAPPS_CLIENT_ID`, `CSIAPPS_CLIENT_SECRET`, `CSIAPPS_REDIRECT_URI`,
    `CSIAPPS_SCOPE`, and additionally `CSIAPPS_SECRET_KEY` for Dash. It does
    **not** need `CSIAPPS_ACCESS_TOKEN`: once you have logged in, the access
    token comes from the web session. Set `CSIAPPS_TEST_SOURCE_UUID` as well, or
    the ingest round-trip skips itself.

    While you are there, eyeball the app: chrome renders, both boards green,
    Registry + Warehouse tabs work.

    **c) Live gate, from the CLI** — optional, and it has a sharp edge:

    ```bash
    CSIAPPS_ENV=production CSIAPPS_ACCESS_TOKEN=<token> \
      .venv/bin/python selftest.py --live
    ```

    `selftest.py` does **not** call `load_dotenv()` — only `app.py` does — so it
    sees nothing but the ambient shell environment. And with no web session,
    `current_token()` falls back to `CSIAPPS_ACCESS_TOKEN`, which is blank in
    the checked-in `.env` files. Both values have to be exported in the shell.

!!! warning "A green `--live` run can mean zero live checks ran"
    The live suite returns early with a **SKIP** row if the app is in sandbox
    mode, and again if no token is available. `SKIP` is not `FAIL`, and the
    self-test exits non-zero only on failures — so a run with every live check
    skipped still prints a tidy summary and exits 0. Always read the
    `N passed, N failed, N skipped` line rather than trusting the exit code.
    This applies to both languages.

Run each app in **sandbox** mode (the default) at minimum; run it in
**production** mode too if the change touches auth, the client, or chrome.

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

    Push the branch / merge to `main`. Nothing is published to PyPI yet — this
    only makes the change available from source.

### 4. Re-validate on the *deployed* dummy apps

The point of this step is to catch what a local working copy can't: real OAuth,
real data, the deployment environment, multi-request behaviour. Point the
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

    !!! note "Nothing to revert afterwards"
        Unlike the Python flow, you never hand-edit a dependency file. The
        manifest is generated, so step 5 simply regenerates it from the release
        tag instead of `staging`.

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

    - Temporarily install `csiapps` **from `main`** rather than the pinned
      release — in the dummy app's `requirements.txt`:

      ```text
      # pre-release validation only; revert to the pinned version before step 5
      csiapps[dash] @ git+https://github.com/CSIOntario/csiapps-py.git@main
      ```

    - Redeploy the dummy app (Posit Connect) and open it.

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

    1. **Bump the version** in `pyproject.toml` (and `__version__`), following
       semver — a breaking public-surface change is a major/minor bump, not a
       patch.
    2. **Publish to PyPI.** There is no release workflow in the repo yet, so
       this is manual: tag the commit, build, and upload.

        ```bash
        git tag v<new version> && git push --tags
        uv build                     # or: python -m build
        uv publish                   # or: python -m twine upload dist/*
        ```

    3. **Update the docs** —
       [`csiapps`](https://github.com/CSIOntario/csiapps), the repo behind this
       site: the [Python reference](api.md) autodocs from `csiapps-py@main`, but
       the tutorials, the [parity checklist](parity.md), and any changed
       behaviour are hand-written and must be edited. Pushing to `main` there
       redeploys the site.
    4. **Bump the pin and redeploy consumers.** Change
       `csiapps[...]==<new version>` in the dummy apps (revert the step-4 git
       line) **and** in every dependent app, then redeploy. The dummy apps
       should be green one more time on the pinned release.

## Checklist

=== "R"

    - [ ] `devtools::document()` run; `man/` and `NAMESPACE` changes committed.
    - [ ] `devtools::test()` and `devtools::check()` green in `csiapps-r`.
    - [ ] `Rscript selftest.R` (sandbox) green in `dummy-r-shiny` against the
          working copy — with the `pkgload::load_all` line confirmed in the log.
    - [ ] The dummy app renders and both boards are green locally, in production
          mode.
    - [ ] Any `selftest.R --live` run checked for skipped rows, not just exit 0.
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
    - [ ] `selftest.py` (sandbox) green in **both** dummy apps against the
          working copy.
    - [ ] Each dummy app renders and both boards are green locally, in
          production mode.
    - [ ] Any `selftest.py --live` run checked for skipped rows, not just exit 0.
    - [ ] Change pushed to `csiapps-py@main`.
    - [ ] Deployed dummy apps green in production (installed from `main`).
    - [ ] Version bumped; published to PyPI.
    - [ ] Docs updated (tutorials / parity / behaviour notes).
    - [ ] `csiapps==<version>` pin bumped and redeployed in the dummy apps and
          every dependent app.

## How the two release processes differ

Everything above follows the same five steps, but the mechanics diverge in ways
worth knowing before you run the loop in the other language:

| | R | Python |
|---|---|---|
| Canaries | `dummy-r-shiny` | `dummy-python-shiny` + `dummy-python-dash` |
| Test command | `devtools::test()` / `devtools::check()` | `uv run pytest` |
| Regenerate docs from source | `devtools::document()` commits `man/` + `NAMESPACE` | not needed — mkdocstrings reads the source |
| Getting the working copy into the dummy app | automatic, `pkgload::load_all("../csiapps")` | `pip install -e '../csiapps-py[extra]'` |
| Env vars for the CLI self-test | `.Renviron`, auto-loaded | must be exported in the shell |
| Integration branch | **`staging`** — see below | none; `main` is the integration branch |
| Deploy pin | generated `manifest.json`, pins an exact `GithubSHA1` | hand-edited `requirements.txt` line |
| Pre-release deploy | `deploy.R` (defaults to `staging`); nothing to revert | temporary `git+…@main` line that **must** be reverted |
| Publishing the deploy | `git push` the manifest; Connect Cloud rebuilds | redeploy the app |
| Distribution | GitHub — no registry | PyPI |
| Effect of merging to `main` | **publishes** to every `install_github()` consumer, and to the pkgdown site | inert until `uv publish` |
| Version file | `DESCRIPTION` | `pyproject.toml` + `__version__` |
| Version bumped at | step 3, on `staging` | step 5 |
| Release notes | GitHub Release body | tag / PyPI release notes |
| Reference docs | separate [pkgdown site](https://csiontario.github.io/csiapps-r/), auto-built from `main` | [mkdocstrings](api.md), inside this site |

!!! question "Why does only R have a `staging` branch?"
    Because in R the git ref *is* the distribution channel. Merging to `main`
    publishes to every `install_github()` consumer and rebuilds the public
    function reference, so `main` cannot double as the place unvalidated work
    lands — `staging` is that place.

    Python already has the separation for free: merging to `csiapps-py@main`
    publishes nothing. `uv publish` is a deliberate, separate act, so `main` is
    a safe integration branch and a staging branch would add ceremony without
    removing a risk.

    The asymmetry is intentional. If the Python flow ever gains an
    auto-publish-on-merge step, revisit it.

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
