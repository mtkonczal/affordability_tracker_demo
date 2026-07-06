# Next Steps 2 — Executing Plans 1–4

Handoff notes from the July 6, 2026 session. Plans 1–4 (see `tracker_plan.md`) were scoped for execution with all new additions going into a **"newest" filter** for version tracking, then paused before code changes. Nothing in the repo has been modified except the planning/log markdown files.

## What was learned this session (environment constraints)

1. **The Cowork sandbox has no R**, and its network allowlist blocks `fred.stlouisfed.org`, `api.bls.gov`, `cdn.jsdelivr.net`, and `unpkg.com` from the shell. `registry.npmjs.org` works (Node 22 available), so JS libraries can be vendored via npm.
2. **Data refresh cannot happen from inside the sandbox.** New series added to `fetch_data.R` get data only when (a) you run `Rscript fetch_data.R` locally, or (b) the Plan 1 GitHub Action runs. The UI already renders only what exists in `data/app_data.js`, so unfetched series simply don't appear — safe to ship config ahead of data.
3. Existing `data/` is from the April 2025 fetch — a refresh should be the first thing the new Action (or a local run) does.

## Design decisions made (carry these forward)

1. **"Newest" filter:** add an `is_new: true` flag to new `SERIES` entries in `fetch_data.R` and a `{ id: 'newest', label: 'Newest', matches: (it) => it.is_new }` entry to `FILTERS` in the front end. This keeps each series' real category intact (a series can be both "Groceries" and "Newest") and makes version tracking a one-flag change when a release graduates.
2. **Front end:** precompile the JSX once (Node/Babel in the sandbox) into an external `js/app.js`, vendor React production builds + Chart.js + date adapter into `vendor/`, drop Babel-standalone and the dev builds. Keeps the current look and behavior exactly; avoids a risky rewrite. Source JSX stays in the repo (e.g., `src/app.jsx`) so future edits recompile cleanly.
3. **Plan 3 debt series should come from FRED, not NY Fed Excel files**, so the existing fetcher works unchanged: `REVOLSL` (revolving credit), `SLOAS` (student loans), `MVLOAS` (auto loans), `DRCCLACBS` (credit card delinquency), `DRCLACBS` (consumer loan delinquency), `DRSFRMACBS` (mortgage delinquency). NY Fed CCP state-level data stays a Plan 2/rankings item.
4. **Plan 4:** add `CPIAUCSL` (all-items CPI) to `SERIES` and compute a derived real-wages series in `fetch_data.R` (nominal AHE ÷ CPI). The hours-to-afford visual can be built from data already on hand (`hourly_earnings` + price series) — `hoursToAfford()` is already stubbed in `index.html`.

## Open decision (blocks Plan 2 only)

Choose scope for state/metro data: **(a) pilot — 5 states × 4 core series + state picker** (recommended; proves the model, small page weight), **(b) metro CPI cards only** (smallest), or **(c) all 50 states** (~200 series, ~2–3MB heavier page, complete). Plans 1, 3, 4 are unblocked regardless.

## Execution order when resuming

1. **Plan 1a — front end:** compile JSX → `js/app.js`; vendor libs; add CSP meta tag; keep `index.html` behavior identical. Verify by opening locally.
2. **Plan 1b — automation:** write `.github/workflows/update-data.yml` (cron ~2nd and 16th, post-CPI; `workflow_dispatch` for manual runs; `permissions: contents: write`; R setup → `Rscript fetch_data.R` → commit `data/`). Add `BLS_KEY` repo secret (GitHub UI — requires you). Enable Pages. Write `DEPLOYMENT.md` with the iframe embed snippet for the ESP site.
3. **Plans 3 + 4 — config:** add debt series (FRED IDs above) with `category = "debt"` + `is_new`, new `debt` entry in `CATEGORIES`/`FILTERS`; add `CPIAUCSL` + derived real wages with `is_new`; unhide `labor`; fix the wage up/down color inversion; add "newest" filter; build hours-to-afford section.
4. **Refresh data:** `Rscript fetch_data.R` locally (or trigger the Action) so newest series go live.
5. **Plan 2** per the scope decision above: `geo` field, state/metro series, picker UI, `rankings.json`.
6. Log everything in `Fable_log.md`.

To resume: decide the Plan 2 scope, then "Execute next_steps_2.md."
