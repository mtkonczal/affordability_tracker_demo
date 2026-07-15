# Verify & Fix: July 2026 feature build

Instructions for an AI agent running in a terminal **with R, Python 3, and
network access** in this repo. A previous session (no R, no network) made
the changes below. Your job: **run the full pipeline, verify every feature
end to end, and fix whatever is broken until all acceptance checks pass.**
Do not stop at reporting a failure — diagnose and fix it, then re-run.
Read CLAUDE.md first; respect its conventions (no bundler, no hand-editing
committed data artifacts, no implying state CPI exists).

## What was changed (context)

1. **NY Fed annual time series** — `scripts/convert_annual.py` now writes
   `data/nyfed/{debt_per_capita,studentloan_per_capita,cc_delinquency_90}.csv`
   (long format `date,code,value`, every Q4 2003→, dated YYYY-12-01, `allUS`
   → `US`). These feed `source = "nyfed"` entries in `SERIES` and
   `STATE_METRICS`. The two old NY Fed single-value tiles were removed from
   `data/annual/annual_meta.json`.
2. **Census ACS series** — new `scripts/fetch_census.py` writes
   `data/census/{income_20th,income_80th,rent_burden}.csv` (B19080 quintile
   upper limits = 20th/80th percentile income; B25070 share paying 30%+).
   Needs `CENSUS_API_KEY`. The matching `SERIES` entries are `optional = TRUE`.
3. **Derived rent-hours** — `rent_hours` = ZORI rent ÷ avg hourly earnings,
   national (in `all_data`) and per state (`source = "derived_rent_hours"`,
   must run after rent + wages in `STATE_METRICS`).
4. **Build-time rankings** — every state metric summary gets `rank`
   (1 = highest latest value, ties "min") and `rank_n`, in both
   `data/states/{st}.js` and `data/state_metrics/{id}.js`.
5. **fetch_data.R plumbing** — `read_kff()` generalized to `read_long_dir()`
   + `LONG_SOURCES` (kff/nyfed/census); `fips` added to `states_index.js`;
   optional entries excluded from the failure exit; new national card
   `water_sewer_trash` (FRED `CUSR0000SEHG`); Groceries CPI card moved to
   the `groceries` category.
6. **Front-end (index.html)** — alphabetized chips/cards; rank badges on
   My State cards (flow into Copy-fact); Compare States: multi-select up to
   4 metrics as side-by-side panels, hash `metric=a,b,c`; all-states section
   gets Grid/Map toggle (D3 choropleth: topojson-client + full d3 bundle +
   us-atlas states-10m from jsDelivr, geoAlbersUsa scale 1300 translate
   [487.5, 305]), Rank sort + #N chips; `fmtVal` handles "Hours of Work".
7. **about.html, CLAUDE.md, .github/workflows/update-data.yml** updated.

## Step 1 — Regenerate all data

```bash
python3 scripts/convert_annual.py     # should print nyfed/… 52 geographies x 23 years
python3 scripts/fetch_census.py       # needs CENSUS_API_KEY (env or .Renviron)
Rscript fetch_data.R                  # full refresh; takes several minutes
echo "exit: $?"                       # MUST be 0
```

Keys live in `.Renviron` (repo root or home): `BLS_KEY`, `CENSUS_API_KEY`
required; `EIA_KEY` optional. `fetch_census.py` reads `CENSUS_API_KEY` from
the environment or `./.Renviron` — if the key is only in `~/.Renviron`,
export it first. If the newest ACS vintage 404s, the script stops cleanly at
the last available year — that is fine; any *other* year failing is not.

If `Rscript fetch_data.R` errors or exits non-zero: read the message, fix
the R code (typos, list-field mistakes, merge issues are the likely
suspects — this code was written without an R interpreter), and re-run
until exit 0. Watch specifically:
- the `LONG_SOURCES` reader and the generalized kff/nyfed/census branches
  (national loop ~line 1130, state loop ~line 1300),
- the `derived_rent_hours` branch (merge of rent + wages data frames),
- the `metric_ranks` block and `summarize_series(res, metric_id, code)`
  call sites,
- the derived national `rent_hours` block after `real_hourly_earnings`.

## Step 2 — Validate the generated payloads

Run these checks (node or python, your choice) and fix the pipeline if any
fail — do not patch the generated files by hand:

- `data/app_data.js` contains ids: `water_sewer_trash`, `groceries`
  (category `"groceries"`, color `#A16207`), `rent_burden`, `income_20th`,
  `income_80th` (both `invert_color: true`), `debt_per_capita` (a card),
  `studentloan_per_capita` + `cc_delinquency_90` (`overlay_only: true`),
  `rent_hours` (units `"Hours of Work"`, data from 2015).
- NY Fed series: US row present, ~23 annual points dated `-12-01`,
  `debt_per_capita` US latest ≈ $60–75k (sanity), state CA > state MS.
- Census series: `income_20th` US 2024 should be ≈ $30–37k, `income_80th`
  ≈ $165–185k (sanity ranges — verify against data.census.gov B19080 if
  outside), `rent_burden` values between 35 and 60 for all states.
- `data/states/ca.js`: every metric has `rank` and `rank_n`; spot-check
  correctness: CA's `rent` rank should be near the top (1–5);
  `rank_n` ≤ 51 and equals the number of states in
  `data/state_metrics/rent.js`.
- `data/state_metrics/rent_hours.js` exists; CA latest ≈ rent ÷ wage from
  the same file's inputs (recompute one state by hand, tolerance 0.1).
- `data/states_index.js`: every state has `fips`; metrics list includes the
  6 new metrics with `n_states > 0`.
- No metric lost states vs. before (e.g. `unemployment` still 51).
- `data/manifest.json` parses and includes the new series.

## Step 3 — Verify the front end in a real browser

Serve statically (`python3 -m http.server` or the `.claude/launch.json`
server) and drive a real browser (Playwright headless Chromium is fine —
`npx playwright` — but do NOT add it to the repo; keep it in a temp dir).
Check, fixing `index.html` where broken:

1. **National**: category rows render alphabetically (Big-ticket, Daily
   items, Debt, Groceries, Work & wages); chips alphabetical within rows;
   Groceries chip sits in the Groceries row; Water & Sewer in Daily items;
   the new cards render (`rent_hours` shows "X.X hrs" — if it shows a `$`,
   `fmtVal`'s Hours branch is broken). Heatmap renders for annual series
   without NaNs.
2. **My State** (e.g. `#view=state&state=IL`): cards alphabetical; every
   card shows a "Rank Nth highest of M" badge; the NY Fed and Census cards
   chart with a dashed US overlay; Copy-fact on a ranked card includes
   "the Nth highest among M states". No console errors.
3. **Compare — multi-metric**: select 4 metrics → 4 panels side by side,
   shared pinned states; hash becomes `metric=a,b,c,d` and reloading that
   URL restores all 4; removing works; a 5th selection replaces the 4th.
4. **Compare — rank sort**: Rank sort orders by #1, #2…; #N chips match
   payload `rank`.
5. **Compare — map**: Map toggle loads the choropleth (real network needed
   for jsDelivr); all 51 jurisdictions filled; hover tooltip shows name,
   value, change, rank; click pins the state (outline appears + it joins
   the overlay chart); Change/Level toggle recolors; invert-color metrics
   (income, wages) color falling values red on the Change map. Test on at
   least: `unemployment` (all 51), `rent` (some states missing — they must
   render gray, tooltip "No data"), `income_20th` (annual).
6. **Real toggle**: `income_20th` deflates under Real; `rent_hours` and
   `rent_burden` do not change (not deflatable); `income` (median) never
   double-deflates.
7. **Anchors**: 2019 anchor on NY Fed metrics uses the Q4-2019 point
   (badge "Since Dec '19" non-null); Census metrics show a value for
   "Since Dec '19" (2019 ACS point) and Jan '25 only if a 2025 point exists
   (it won't — badge may be absent; absent is correct, NaN is not).
8. Take screenshots of each view for the final report.

## Step 4 — Regression sweep

- `open about.html` — new source rows render; no broken table markup.
- CSV/PNG buttons still download on a national card and a state card.
- Deep link from CLAUDE.md (`#view=state&state=IL&anchor=2019&type=real`)
  still works.
- `git diff data/` should show only regenerated artifacts; commit nothing
  by hand-editing.
- Re-run `Rscript fetch_data.R` a second time: it must be idempotent
  (only `last_updated`/`updated` stamps and any newly-released data change).

## Definition of done

Every check in Steps 1–4 passes in a real browser with freshly generated
data, `Rscript fetch_data.R` exits 0 twice in a row, and no browser console
errors on any of the three views. Then write a short summary of what you
fixed (if anything) into `VERIFY_RESULTS.md`, including the sanity values
you observed (US debt per capita, US income 20th/80th, CA rent rank,
CA rent_hours), and delete nothing else. If something cannot be made to
work (e.g. a source is down), say so explicitly in `VERIFY_RESULTS.md`
rather than papering over it.
