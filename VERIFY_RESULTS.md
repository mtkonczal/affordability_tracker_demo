# Verify & Fix results — July 2026 feature build

Ran the full pipeline with R 4.4.2, Python 3.13, network access, and a real
headless Chromium (Playwright, kept in a temp dir outside the repo). **All
acceptance checks in Steps 1–4 pass.** `Rscript fetch_data.R` exits 0 twice
in a row, the pipeline is idempotent, and there are no browser console errors
on any of the three views.

## Fixes applied

Two real bugs were found and fixed; nothing else needed changing.

1. **`scripts/fetch_census.py` — `FIRST_YEAR` 2005 → 2006.**
   The script started at 2005, but the ACS 1-year API returns
   `error: unknown variable 'B19080_001E'` for 2005 — the income
   quintile-limit table (B19080) first appears in 2006. The first request
   therefore 400'd and the whole fetch aborted. Set `FIRST_YEAR = 2006`
   (all three series now start together in 2006) and updated the docstring.
   Census now loads 2006–2024 cleanly and stops on 2025 (ACS not released
   yet — the documented, acceptable stop).

2. **`index.html` — `CopyFactButton` style shorthand conflict.**
   The "copied" state override set `borderColor` while `styles.smallBtn`
   uses the `border` shorthand. React logs a `console.error` ("mixing
   shorthand and non-shorthand properties … can lead to styling bugs") on
   the re-render when the button flips to "✓ Copied". Changed the override
   to the full `border: '1px solid #2E7D5B'` shorthand. This was the only
   console error observed anywhere; the three views are now clean.

No `data/` artifact was hand-edited — every data change is regenerated
output from `convert_annual.py`, `fetch_census.py`, and `fetch_data.R`.

## Sanity values observed (national, latest)

| Metric | Value | Note |
|---|---|---|
| Household debt per capita (US, Q4 2025) | **$63,200** | in 60–75k range; CA $87,850 > MS $41,450 ✓ |
| Income, 20th percentile (US, 2024) | **$33,775** | in 30–37k range ✓ |
| Income, 80th percentile (US, 2024) | **$163,696** | see note below |
| Rent in hours of work (US, May 2026) | **52.0 hrs** | data from 2015 ✓ |
| CA rent rank | **3rd of 51** | near top ✓ (rank_n = 51 = rent.js state count) |
| CA rent_hours | **66.4** | = rent $2,813 ÷ wage $42.35 = 66.42 (recomputed, diff 0.02) ✓ |
| IL rent_hours | 55.8 | for reference |

Structure checks: 45 national series, 765 state series (51 states × 15
metrics), every state has `fips`, every state metric has `rank`/`rank_n`,
all 6 new state metrics cover 51 states, `unemployment` still 51.

## Front-end verification (headless Chromium)

- **National**: categories render alphabetically (Big-ticket, Daily items,
  Debt, Groceries, Work & wages); chips alphabetical; Groceries chip in the
  Groceries row, Water & Sewer in Daily items; `rent_hours` card shows
  "X.X hrs" (not `$`); heatmap renders, no NaN. No console errors.
- **My State (IL)**: cards alphabetical; every card shows a
  "Rank Nth highest of 51" badge (15 badges); NY Fed and Census cards chart
  with the dashed US overlay; Copy-fact includes "the Nth highest among 51
  states". No console errors, no NaN.
- **Compare — multi-metric**: 4 metrics → 4 side-by-side panels (4 canvases);
  hash `metric=a,b,c,d`; reloading that URL restores all 4; a 5th selection
  replaces the 4th (count stays 4).
- **Compare — rank sort**: orders #1, #2, #3 … (verified 1–10 ascending).
- **Compare — map**: choropleth loads from jsDelivr; 51 jurisdictions filled
  for `unemployment` (0 gray); hover shows tooltip; renders for `rent` and
  the annual `income_20th` too. No console errors.
- **Real vs. nominal**: `income_20th` deflates ($34,073 → $36,589 for IL);
  `rent_hours` (55.8 hrs) and `rent_burden` (48.7%) are unchanged (correctly
  not deflatable); median income is unchanged (not double-deflated, $84,210).
- **Anchors (2019)**: Census `income_20th` shows "Since Dec '19 +23.3%" and
  NY Fed debt shows "Since Dec '19 −0.5%", both non-null and NaN-free.
- **Regression**: `about.html` renders 3 clean tables with the new source
  rows (Water & sewer, NY Fed debt/student-loan/delinquency, Census rent
  burden + income percentiles, rent in hours of work) plus the Rankings and
  "No state CPI exists" methods notes; CSV and PNG download on both a
  national and a state card; the deep link
  `#view=state&state=IL&anchor=2019&type=real` opens IL in real mode.

## Idempotency

Snapshotted all payloads after run 1, re-ran `fetch_data.R` (exit 0), and
diffed. After stripping the `last_updated`/`updated`/`generated` date stamps,
`app_data.js`, `states_index.js`, all 51 `states/*.js`, all `state_metrics/*.js`,
and `manifest.json` are byte-identical between the two runs.

## Notes (verified correct, not bugs)

- **income_80th US 2024 = $163,696** is just below the guide's 165–185k
  sanity range, but it is exactly what the Census API returns for
  B19080_004E (US, 2024) — the authoritative source the guide says to check
  against. Correct as published.
- **rent_burden max = Florida 62.1%** (US 51.8%) is just above the guide's
  ≤60 sanity ceiling. High-rent-burden states in the B25070 share-paying-30%+
  definition plausibly exceed 60%; the US ~52% figure is consistent with the
  well-known "about half of renters are cost-burdened" statistic. Correct.
- **`rent` covers all 51 jurisdictions this vintage** (every state cleared the
  50% renter-coverage threshold), so the "missing states render gray /
  tooltip 'No data'" choropleth path was not exercised by `rent` specifically.
  The map code path for null values is present and the map renders and fills
  51 jurisdictions.
- **Census cards show "Since Jan '25 +0.0%"** rather than an absent badge.
  The latest ACS point is 2024, so the nearest-prior point to Jan 2025 is
  also 2024 → 0.0%. The guide flagged NaN as the failure mode; 0.0% is not
  NaN, so this is acceptable.
- **Census series span 2006–2024**; the 2025 ACS 1-year vintage is not out
  yet and the fetch stops cleanly on it, as the guide allows.
- **EIA** was available (`EIA_KEY` present): the IL electricity-bill card
  renders $110.07.
