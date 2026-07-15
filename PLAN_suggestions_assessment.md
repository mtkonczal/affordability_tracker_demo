# Assessment of Next_suggestions_for_ESP_tracker.md

*July 2026. Feasibility calls based on the current codebase, data pipeline,
and what's actually available from our sources (FRED, BLS, Census, Zillow,
EIA, KFF, NY Fed, Urban).*

## Do first — high value, data already in hand

**1. State ranking column (High priority #1).** Fully feasible; every
state metric's latest values already sit in `data/state_metrics/{id}.js`.
Compute ranks at build time in `fetch_data.R` (latest value + change since
each anchor + gap vs. US) and write them into the state payloads. This was
Phase 3 of NEXT_VERSION.md, descoped by decision — this feedback re-opens
it, and it powers the core comms sentence ("STATE ranks 47th").
One caveat: "population-weighted alternative rankings" needs clarification —
a rank is a rank. Likely intent is either (a) population-weighting the US
average line, or (b) a "share of Americans living in states worse than
yours" stat, which we can compute from Census population. Ask before
building.

**2. Debt time series (Low priority #1) — but use NY Fed, not Urban.**
The raw NY Fed State-Level Household Debt file already committed at
`data/annual/raw/area_report_by_year.xlsx` contains **annual series
Q4 2003 – Q4 2025** for total debt, auto, credit card, mortgage, student
loan balances per capita, plus delinquency rates — all 50 states + DC.
That beats Urban's 2018+ window, and Urban's own technical appendix warns
its series isn't comparable over time (medical-debt reporting changes in
2022–23, methodology shifts). On their source-consistency question: NY Fed
CCP/Equifax covers both US and states from the same panel, so we get
consistency for free. Implementation path exists: promote these from
single-value stat tiles to annual time series exactly the way KFF series
work (`source = "kff"` pattern in `SERIES`/`STATE_METRICS`). Keep Urban
for what it's uniquely good at: the medical-debt and collections *levels*
stat tiles.

**3. Alphabetical ordering (High priority #3).** Trivial front-end sort.
Recommend alphabetizing *within* categories but keeping category grouping
and color families — comms explicitly praised color coordination.

**4. Groceries chip cleanup (Low priority #2).** Trivial. Move the
"Groceries" (food-at-home CPI) card from Daily Items into the Groceries
category as its headline series rather than deleting it — it's the
aggregate the individual items roll up to, and the one press releases cite.

## Feasible with design decisions — medium effort

**5. Income threshold / burden column (Medium #1).** No off-the-shelf
"burden" series exists, but three defensible measures are computable from
data we already fetch or can add cheaply:
- **Rent burden** — ACS B25070 (share of renters paying 30%+ of income),
  all states, via the Census API we already key. The canonical stat.
- **Rent-to-income ratio** — ZORI rent × 12 ÷ median household income,
  both already in the pipeline.
- **Hours of work** — monthly rent ÷ state average hourly wage (both in
  pipeline). This is the comms "stopwatch" idea made rigorous.
Recommend B25070 + hours-of-work; the ratio is redundant with B25070 but
weaker (asking rent vs. actual rents paid).

**6. 25th/75th percentile income (Medium #2).** Not directly published.
Closest real product: **ACS table B19080** (household income quintile
upper limits = 20th/40th/60th/80th percentiles), all states, annual, via
the Census API — [table exists back to at least 2016 in the
API](https://api.census.gov/data/2016/acs/acs5/groups/B19080.html).
Options, in order of defensibility:
1. Use **20th and 80th percentiles** and label them honestly. Recommended.
2. Interpolate 25th/75th from the quintile limits — adds distributional
   assumptions that won't survive a hostile fact-check.
3. Compute exact percentiles from ACS PUMS microdata — accurate but a
   heavy yearly job; not worth it for two extra percentile points.

**7. Multi-metric Compare (High priority #2).** Pure front-end. The data
layer is already built for it — `data/state_metrics/{id}.js` files
lazy-load independently, so loading up to four is cheap. Add a small-
multiples grid mode to `CompareView` (pick up to 4 metrics, one panel
each, pinned states shared across panels). Moderate lift, no pipeline
changes.

**8. Interactive US heatmap / choropleth (Low #4).** Feasible — all data
is static and per-state, no live updates needed, and it was in the
original Phase 3 before descoping. Do it with an inline SVG state map
(no new library; keeps the no-bundler rule). Put it inside Compare
States as a third display mode (overlay / sparklines / map) rather than
a new tab, with click-a-state → deep link to that state's view. This is
the biggest front-end lift on the list; schedule it last.

## Can't work as asked — say no or substitute

**9. State water bills (Low #3).** No free public state-level water-rate
series exists: AWWA's rate survey is proprietary, EIA doesn't cover
water, and BLS publishes no state utility CPIs (no state CPI of any
kind — a line we already hold on the About page). Substitute: the
national CPI for **water/sewer/trash** (BLS, on FRED) as a Daily Items
card, and note the state gap honestly.

**10. Urban debt data for the "live" visualization (Low #1).** Covered
above — Urban's series has documented comparability breaks and a shorter
window; NY Fed is strictly better here. Don't mix Urban into time series.

## Suggested order

| Step | Items | Effort |
|---|---|---|
| 1 | Alphabetize + groceries move (#3, #4) | Hours |
| 2 | Build-time rankings + rank column (#1) | Days |
| 3 | NY Fed debt time series (#2) | Days |
| 4 | Burden metrics + 20th/80th income (#5, #6) | Days (one Census addition) |
| 5 | Multi-metric compare (#7) | ~A week |
| 6 | SVG choropleth (#8) | Biggest lift; last |

Open questions for the team before building: what "population-weighted
rankings" should mean (#1), and whether 20th/80th percentiles are an
acceptable stand-in for 25th/75th (#6).
