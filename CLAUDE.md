# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this project is

A static, self-contained affordability tracker dashboard. `fetch_data.R` pulls FRED time series via the public CSV endpoint (no API key) and writes them into `data/`; `index.html` reads `data/app_data.js` and renders tabs of charts/scorecards using Chart.js loaded from a CDN. There is no build step, no server, and no JS package manager — open `index.html` directly in a browser.

`affordability_tracker.html` (untracked) is a separate ESP-branded variant of the dashboard. The canonical app is `index.html`.

## Common commands

```bash
# Refresh all data from FRED (writes data/*.csv, data/manifest.json, data/app_data.js)
Rscript fetch_data.R

# View the dashboard
open index.html
```

`fetch_data.R` only depends on `jsonlite` (auto-installs if missing) and base R `read.csv` against `https://fred.stlouisfed.org/graph/fredgraph.csv?id=...`. No FRED/BLS/BEA API keys are used here despite the global CLAUDE.md mentioning them.

## Architecture

### Data flow

1. `SERIES` list in `fetch_data.R` declares each indicator: `id`, `fred_id`, `label`, `subtitle`, `category`, `units`, `description`, `color`, `from`.
2. For each series, the script downloads the FRED CSV, writes `data/<id>.csv` (used by the in-page Download button), computes `yoy_change` via `yoy_pct()` (latest value vs. value ≤365 days prior), and accumulates a metadata + data record.
3. Two outputs are written:
   - `data/manifest.json` — metadata only, no data arrays. Lightweight inspection.
   - `data/app_data.js` — assigns the full payload to `window.AFFORDABILITY_DATA` for the HTML to consume via `<script src="data/app_data.js">`.

### Front-end

`index.html` is a single self-contained file (~1100 lines: CSS, markup, JS). Two configuration sources drive everything:

- **`SERIES` in `fetch_data.R`** — what data exists and how each series is labeled/colored.
- **`CATEGORIES` object in `index.html`** (around line 577) — the tab grouping. Each entry has `label`, `subtitle`, `accent`, optional `hidden: true` to suppress an unfinished tab. `CATEGORY_ORDER` is `Object.keys(CATEGORIES)` and controls tab/scorecard ordering.

A series's `category` field in `SERIES` must match a key in `CATEGORIES`. Tabs hide automatically if `hidden: true` or if no series reference them. The `labor` category is currently hidden; flip `hidden: false` to activate it.

### Adding a new indicator

1. Append an entry to `SERIES` in `fetch_data.R` with a new `id`, the FRED series identifier, and a `category` matching an existing or new `CATEGORIES` key.
2. If introducing a new category, add it to `CATEGORIES` in `index.html` (and optionally a `--c-<name>` CSS accent variable around line 38).
3. Run `Rscript fetch_data.R`. No HTML edits are needed for new series within an existing category.

### Notes for editing

- The HTML is hand-written and self-contained; do not introduce a bundler or framework unless asked.
- Chart.js is pinned to `4.4.0` via jsDelivr; the date adapter is `chartjs-adapter-date-fns@3.0.0`. Both load from CDN.
- `data/app_data.js` and `data/*.csv` are committed regenerated artifacts — re-run the R script rather than hand-editing.
- The hidden `labor` category already has three series (`unemployment`, `hourly_earnings`, `job_openings`) wired up in `SERIES` and ready to display.
