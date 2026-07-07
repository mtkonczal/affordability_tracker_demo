# Affordability Tracker v2 — Build Notes & Observations

*Written July 7, 2026, at the end of the v2 build session. This is the
handoff document: what was built, what was verified, what to watch out for,
and what remains manual.*

## What was built

- **Three-view front end** (`index.html`, single file, no build step):
  - **National** — all national series as selectable chart cards plus the
    year-by-year heatmap. Category pills are additive bulk-toggles; every
    chip stays visible (grayed when off) so you can mix categories without
    re-clicking.
  - **My State** — every state metric for one state, each charted against a
    dashed US-average line, plus six annual stat tiles (childcare, ACA
    premium, debt burdens).
  - **Compare States** — one metric across all states: US + click-to-pin
    overlay chart, and a sortable sparkline grid of all 51 jurisdictions
    (A–Z or biggest change since the active anchor).
- **Three message anchors** everywhere: Since Jan 2025, Pre-Pandemic
  (Dec 2019), Since 2000 (plus 1Y and Max). Every card also shows dual
  badges — change since Jan 2025 and since Dec 2019 — regardless of the
  chart window. Global $-levels vs %-change toggle.
- **Copy fact** button on every card produces a press-ready, fully sourced
  sentence, e.g.: *"Connecticut — Rent (Market): $2,231 (May 2026) — up 3.8%
  since January 2025, up 50.8% since before the pandemic (Dec 2019). Source:
  Zillow ZORI (county data, renter-weighted), via the Economic Security
  Project Affordability Tracker."*
- **Deep links** via URL hash (`#view=state&state=IL&anchor=2025`) so a
  campaigner can send a colleague straight to their state and framing.
- **State data system**: 50 states + DC × 6 metrics = 306 series
  (unemployment, hourly wages, FHFA home prices, real median income, market
  rent in dollars, average electricity bill in dollars). Payloads are split:
  `data/app_data.js` (national, ~460K) loads up front; `data/states/{st}.js`
  and `data/state_metrics/{id}.js` lazy-load per view.
- **Annual state layers** (`data/annual/`): KFF ACA benchmark premium
  (plan year 2026), CCAoA infant childcare price (2025), NY Fed household
  debt per capita and 90-day credit-card delinquency (Q4 2025), Urban
  Institute medical-debt and any-debt-in-collections shares (Aug 2025).
  Raw source files committed under `data/annual/raw/`; regenerate with
  `python3 scripts/convert_annual.py`.
- **About the Data page** (`about.html`): every source, cadence, and
  methodological caveat, written to survive a hostile fact-check.

## Data-integrity catches (the important ones)

1. **Urban encodes suppression as zero.** In the Debt in America medical
   file, the seven states that restrict medical-debt credit reporting
   (CA, CO, IL, NY, RI, VT, WA) carry a literal `0` — not a 0.0% share.
   Publishing that would have been a fact-check failure. The converter now
   drops zeros; those states show no medical-debt tile and the About page
   explains why. **General lesson: check what a source's zeros mean.**
2. **Connecticut's rent was silently missing.** Zillow's county file still
   uses CT's legacy county FIPS (09001–09015) while ACS 2023 reports CT as
   planning regions (09110+), so the renter-weight join matched nothing and
   coverage computed to 0%. Fixed by pulling CT weights from ACS 2021, the
   last vintage on legacy counties. If other states redraw county
   equivalents, the same failure mode applies — the symptom is the pipeline
   log line `ZORI: skipping XX (coverage 0%)`.
3. **No state ZORI exists.** Zillow publishes county and metro rent indexes
   but not state. State rents here are county ZORI aggregated with fixed ACS
   renter-household weights; states under 50% renter coverage are dropped
   rather than shown thin (currently none are), and coverage is displayed on
   each state's rent card.
4. **FRED's CDN rejects custom user agents.** Setting
   `options(HTTPUserAgent = ...)` in R broke every FRED fetch with an
   HTTP/2 framing error while BLS and Zillow kept working. Zillow's *static
   CSV host* accepts R's default UA fine (only zillow.com's marketing pages
   bot-block). Don't set a global UA in `fetch_data.R`.
5. **`EIA_KEY` was already in `.Renviron`** (it wasn't in the documented key
   list), so state electricity bills are live. The GitHub Action needs the
   same key added as a secret or the deployed data won't include them.
6. **CCAoA childcare gaps**: CO, DC, NM, SC reported no 2025 infant price
   (no tile). Seven states (AL, FL, MT, PA, TX, WV, WY) carry a `*` footnote
   in CCAoA's published tables whose text isn't machine-retrievable — check
   the report PDF before building a campaign around those specific states.
7. **jsonlite renders R `NULL` as `{}`**, which is truthy in JavaScript.
   Optional fields (like `fred_id` on non-FRED series) must be stripped with
   `compact()` before `toJSON()` or the front end renders junk.

## Verification results (checked in a live browser)

- All three views render with zero console errors; deep links work on fresh
  loads (note: same-document hash edits don't re-parse — by design, links
  are for sharing).
- Sanity-checked values: groceries +32.9% since Dec 2019; Ohio wages $33.88
  (+2.6% since Jan 2025); CA rent $2,813; Montana rent +77.1% since
  pre-pandemic (tops the biggest-change sort, consistent with the known
  Mountain West surge); Ohio annual tiles (ACA $513/mo, childcare
  $13,936/yr, debt per capita $46,780).
- Mobile (375px): no horizontal overflow; controls wrap cleanly.
- Known cosmetic quirk: on weekly series (gas, mortgage), the chart headline
  and the pre-pandemic badge can differ by <1pp — the chart rebases from the
  first observation *on/after* Dec 1, 2019, the badge measures from the last
  observation *on/before* it. Both are internally correct and labeled.

## Automation (runs on the 1st of each month)

`.github/workflows/update-data.yml` runs `Rscript fetch_data.R` on a cron
schedule and commits `data/` only if everything fetched cleanly (any failure
→ non-zero exit → no commit → site keeps serving last good data).

- Schedule: `0 13 1 * *` (~9am ET on the 1st). GitHub cron is UTC and can
  drift 15–60 min at busy times; the exact hour doesn't matter here.
- Trade-off to know: most BLS price data (CPI, average prices) releases
  mid-month. A 1st-of-month run publishes the *prior* release — data will be
  2–4 weeks behind the freshest print until the next run. If comms wants CPI
  day covered, add a second date back: `0 13 1,15 * *`. One-line change.
- Manual refresh any time: Actions tab → "Update affordability data" → Run
  workflow; or locally `Rscript fetch_data.R` + commit `data/`.
- Annual layers are deliberately outside the automation: once a year,
  refresh `data/annual/raw/`, run `python3 scripts/convert_annual.py`,
  update vintages in `annual_meta.json` (see `data/annual/README.md`).

## R vs Python for the pipeline

Recommendation: **keep R.** Reasons:

- GitHub's architecture doesn't meaningfully favor either. Python is
  preinstalled on runners; R needs `r-lib/actions/setup-r` (~1–2 min with
  the RSPM binary cache). At 12 runs/year the difference is minutes per
  year, and runner minutes are free on public repos.
- The pipeline's runtime is dominated by ~250 polite HTTP fetches, not
  language speed.
- The R script is proven, declarative (every series ID explicit), and
  fail-loud. A rewrite risks subtle changes to empirical output (rounding,
  date handling, weighting) for zero user-facing benefit.
- Maintenance realism: this codebase's owner works in R. The one place
  Python earns its keep — parsing the NY Fed xlsx — is already a Python
  script (`scripts/convert_annual.py`), run yearly by hand.

Revisit only if the workflow grows needs R handles poorly (e.g., scraping
JS-rendered pages), or if the R Action setup starts flaking in CI.

## Manual steps that remain (repo admin)

1. Add GitHub Actions secrets: `CENSUS_API_KEY` (required — the run fails
   without it), `EIA_KEY` (optional but recommended; already in local
   `.Renviron`). `BLS_KEY` should already exist.
2. Review and commit the working tree; push to deploy (GitHub Pages serves
   the repo root).
3. Trigger one manual workflow run (Actions → Run workflow) and confirm it
   goes green end-to-end on the runner.
4. Once a year: refresh the annual layers (see above).

## Deferred by decision

- Phase 3 (state rankings, report cards, choropleth) — descoped.
- Phase 5 (what-moved-this-month feed, release calendar, embeds, social
  templates) — deferred; the copy-fact button shipped early.
- Production React builds + precompiled JSX — the page still ships dev
  builds and compiles JSX in-browser via Babel standalone. Fine for review;
  worth doing before a public launch for mobile load times.
