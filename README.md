# Affordability Tracker

A dashboard of national and state-level cost-of-living indicators, built by
the [Economic Security Project](https://economicsecurityproject.org). It
tracks what everyday essentials cost, from groceries and gas to rent, health
insurance, childcare, and household debt, and how those costs have changed
since before the pandemic.

The site is fully static: a single `index.html` renders the dashboard from
pre-built data files in `data/`, so it can be served from GitHub Pages (or
any static host) with no backend. A scheduled GitHub Action refreshes the
data monthly.

## What it shows

The dashboard has four views:

- **National** — selectable chart cards for every national series, grouped
  by category (daily costs, groceries, big-ticket items, labor market,
  debt), plus a year-over-year heatmap. Every card shows change since
  January 2025 and since December 2019.
- **My State** — all available metrics for one state, each with its national
  rank, plus annual stat tiles (childcare cost, medical debt, debt in
  collections).
- **Compare States** — up to four metrics side by side, with pinned states
  overlaid on each panel.
- **Map** — a choropleth (or sorted bar chart) of any state metric, with a
  year selector and a pinned-states time-series chart below.

Global controls let readers measure from different anchors (1 year, January
2025, December 2019, 2000, full history), toggle nominal vs. real (CPI-U
deflated) dollars, and switch between levels and percent change. Every chart
has copy-fact, CSV, and PNG export buttons, and every view is deep-linkable
via the URL hash (e.g. `#view=state&state=IL&anchor=2019&type=real`).

## How it works

```
fetch_data.R  ──►  data/*.js, data/*.csv  ──►  index.html (React + Chart.js via CDN)
```

1. [fetch_data.R](fetch_data.R) pulls every time series from its source
   (FRED, the BLS API, Zillow, the Census API, EIA) and writes static
   JavaScript payloads into `data/`: national series, a state/metric
   catalog, per-state files, and per-metric files (the last two are
   lazy-loaded by the front end).
2. [index.html](index.html) is a self-contained, hand-written page (React
   18 + Chart.js 4 from CDNs, no build step, no package manager) that reads
   those payloads.
3. [about.html](about.html) is a static sources-and-methods page.

State rents are Zillow county-level ZORI aggregated to states with ACS
renter-household weights; states with under 50% renter coverage are omitted.
Annual series (KFF ACA benchmark premiums, NY Fed household debt, Census ACS
income percentiles and rent burden) are fetched by the Python scripts in
`scripts/` and folded in by `fetch_data.R`.

If any series fails to fetch, the script exits non-zero after attempting
everything else, the scheduled Action skips its commit, and the site keeps
serving the last good data: stale, never broken.

## Repository layout

| Path | What it is |
|---|---|
| `index.html` | The dashboard (single file, all four views) |
| `about.html` | Sources and methods page |
| `fetch_data.R` | Data pipeline: fetches all series, writes `data/` |
| `scripts/` | Yearly fetchers (KFF, Census ACS) and the annual-data converter |
| `data/` | Committed build artifacts (JS payloads + CSVs) and raw annual inputs |
| `assets/` | ESP logo assets |
| `.github/workflows/update-data.yml` | Monthly automated data refresh |
| `DEPLOYMENT.md` | GitHub Pages setup, embed instructions, secrets |
| `CLAUDE.md` | Developer/agent guide to the architecture |

All files in `data/` are generated. Re-run the pipeline rather than editing
them by hand (raw inputs in `data/annual/raw/` are the exception).

## Updating the data

Monthly refresh (also runs automatically on the 1st of each month):

```bash
Rscript fetch_data.R
```

Yearly refreshes, when new vintages are released:

```bash
# KFF ACA benchmark premium + uninsured rate (no key needed)
python3 scripts/fetch_kff.py

# Census ACS: 20th/80th percentile income + rent burden (September release)
python3 scripts/fetch_census.py

# Annual stat tiles + NY Fed debt series (after downloading the new
# NY Fed area report into data/annual/raw/)
python3 scripts/convert_annual.py
```

After any of the yearly scripts, re-run `Rscript fetch_data.R` to fold the
results into the site payloads.

### API keys

Keys are read from the environment (or `.Renviron` locally; repository
secrets in CI):

- `BLS_KEY` (required) — seasonally adjusted CPI subindexes not on FRED.
  Free: https://data.bls.gov/registrationEngine/
- `CENSUS_API_KEY` (required) — renter weights and ACS series.
  Free: https://api.census.gov/data/key_signup.html
- `EIA_KEY` (optional) — state electricity bills; skipped loudly if absent.
  Free: https://www.eia.gov/opendata/

## Data sources

| Source | Used for |
|---|---|
| FRED (St. Louis Fed) | Most national and state series (prices, wages, unemployment, home prices, income) |
| BLS API | Seasonally adjusted CPI subindexes and average prices |
| Zillow (ZORI) | Rents, aggregated from counties to states |
| Census Bureau (ACS) | Renter-household weights, income percentiles, rent burden |
| KFF State Health Facts | ACA benchmark premiums, uninsured rate |
| NY Fed Consumer Credit Panel | Household and student debt per capita, credit-card delinquency |
| EIA | State residential electricity bills (optional) |
| Child Care Aware of America | Annual infant childcare cost |
| Urban Institute | Debt in collections, medical debt in collections |

Full series-by-series details, including units and transformations, are on
the [About page](about.html).

## Methodology notes

- "Since 2019" comparisons anchor to **December 2019**, avoiding COVID-era
  base effects.
- Real values deflate by CPI-U (all items), expressed in latest-month
  dollars. State series use the national CPI-U: **no state-level CPI
  exists**, and the tracker does not imply one.
- Index-level series (e.g. home price indexes) always display as cumulative
  percent change, since index levels are not comparable across states.
- State rankings are computed at build time on each metric's latest value.

## Running locally

```bash
open index.html
```

No build step or server required. See [DEPLOYMENT.md](DEPLOYMENT.md) for
GitHub Pages setup and for embedding the tracker on another site via
`?embed=1`.
