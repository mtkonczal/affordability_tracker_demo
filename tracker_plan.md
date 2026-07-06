# Affordability Tracker — Work Plan

Each item from Step 2 of the planning session (see `Fable_log.md`) is its own numbered plan. Plan 1 is the deployment work; Plan 9 collects the site-quality recommendations. Nothing here is executed yet.

---

## Plan 1 — Deployment: self-updating, secure, embeddable

**Goal:** ESP drops one iframe line into its CMS; the tracker updates itself with zero maintenance.

1. Consolidate the front end: precompile the JSX (or fold back to plain Chart.js per CLAUDE.md); vendor Chart.js + date adapter into the repo; remove React dev builds and Babel standalone.
2. Add a Content-Security-Policy meta tag (scripts from self only).
3. Write `.github/workflows/update-data.yml`: cron on the 2nd and 16th (post-CPI mornings), setup-r, `Rscript fetch_data.R`, commit `data/` changes. Permissions: `contents: write` only.
4. Add `BLS_KEY` as a repo Actions secret; enable failure email notifications.
5. Enable GitHub Pages on `main`; optionally CNAME `affordability.economicsecurityproject.org`.
6. Give ESP's web team the sandboxed `<iframe>` snippet; test in the CMS.
7. Dry-run a failure (bad series ID) to confirm the site keeps serving last-good data.

**Effort:** 1–2 days. **Dependencies:** none — do first; everything else rides on it.

## Plan 2 — State and metro data (Feedback A)

**Goal:** a state picker so groups can cite their own state, within the real limits of federal data (no state CPI exists).

1. Add a `geo` field to `SERIES` in `fetch_data.R`; keep national as `geo = "us"`.
2. Add metro CPI series (all-items, food, shelter) for the ~21 published metros; note 2025 BLS collection cuts make these noisier.
3. Add census-division CPI as the fallback mapping for every state.
4. Add true state-level FRED series: unemployment (LAUS), avg hourly earnings (state CES), median household income (ACS), FHFA house price index, Zillow rent index (ZORI), EIA electricity prices, EIA gas (selected states).
5. Build a separate annual `rankings.json`: rent burden (ACS), medical debt in collections (Urban Institute), delinquencies by state (NY Fed CCP), childcare prices (DOL), health premiums (KFF/MEPS-IC).
6. Add a state picker to the UI that swaps series and shows the state's ranks.

**Effort:** the big lift — 2–3 weeks. **Dependencies:** Plan 1.

## Plan 3 — Debt indicators (Feedback B)

**Goal:** a "Debt" tab with quarterly national (later state) debt and delinquency data.

1. Add a `debt` category to `CATEGORIES` and fetchers for NY Fed Household Debt & Credit (credit card, student, auto balances; delinquency rates — quarterly).
2. Add Urban Institute medical-debt-in-collections (annual).
3. Chart delinquency rates prominently — Q1 2026 credit card delinquency is a 16-year record; student loan delinquency is at 10.3%.
4. State-level versions fold into Plan 2's rankings file.

**Effort:** ~1 week. **Dependencies:** Plan 1; NY Fed files need a small non-FRED fetcher.

## Plan 4 — Real income / wage gap (Feedback C)

**Goal:** show the broken-incomes side, not just prices.

1. Unhide the `labor` category (`hidden: false`).
2. Add a real (CPI-deflated) hourly earnings series computed in `fetch_data.R`.
3. Build the two visuals comms sketched: stacked bills (combined childcare + health + groceries + rent vs. median income) and the hours-of-work-to-afford stopwatch — `hoursToAfford()` and reference prices are already stubbed in `index.html`.
4. Fix the wage color inversion first (see Plan 9.1) so rising wages don't render as bad news.

**Effort:** ~1 week. **Dependencies:** none beyond Plan 1.

## Plan 5 — Trump prices (Feedback D)

**Goal:** one-click "since Trump took office" framing.

1. Rename the "Since 2025" range to "Since Jan 2025" and anchor it to 2025-01-20.
2. Add a scorecard row: change since inauguration for every series.
3. Decide whether it becomes the default range (recommend yes for launch).

**Effort:** hours. **Dependencies:** none.

## Plan 6 — Prices vs. income metrics (Feedback E, nice-to-have)

**Goal:** "% of income" and "hours of work" comparisons.

1. National version now: cost basket ÷ median household income; hourly-wage-to-price ratios.
2. State version after Plan 2 (ACS income + state costs): rent burden rankings, "avg worker works N hours for X."

**Effort:** a few days after Plans 2/4. **Dependencies:** Plans 2 and 4.

## Plan 7 — Sample content and infographics (Feedback F)

**Goal:** people use the tracker because we hand them the words and images.

1. "Copy this stat" button per card: sentence + number + citation, one click.
2. Press-release and social-copy templates with numbers auto-filled from live data (mad-libs strings generated at build time).
3. Batch-render infographic PNGs (and later 50 state report cards) in the Actions workflow via headless Chrome — front-loads infographics before launch as comms asked.
4. State report card template (fed by Plan 2 rankings).
5. AI prompt templates for groups to generate their own local content.

**Effort:** ~1 week, mostly writing; can run parallel to Plans 2–4. **Dependencies:** Plan 1 (for #3), Plan 2 (for #4).

## Plan 8 — UX fixes from Erion (Feedback G)

**Goal:** clear the friction a real user already hit.

1. Additive filters: unselected chips stay visible grayed-out; adding Groceries to Big Ticket no longer resets the selection; category pills stop wiping choices.
2. Color-coordinate series by category family (groceries in one hue range, etc.).
3. %↔$ toggle per chart (rebase helpers already exist).
4. One decimal place on percentages.

**Effort:** 1–2 days. **Dependencies:** none — do alongside Plan 5 in the first pass.

## Plan 9 — Site quality and reporter tools (from Step 3 judgment)

1. Invert up/down colors for the labor category — wage growth is good news.
2. Shareable URLs: encode selection + range in the hash.
3. Per-chart iframe embed snippets for partners.
4. Methodology page + visible release calendar ("Next update: CPI day, July 15").
5. Prices-vs-wages overlay as the hero chart — the gap is the story.
6. Compute the hardcoded hero copy and reference prices from data so they can't rot.
7. Heatmap: trailing-12-month column instead of Jan-over-Jan; red/blue diverging scale.
8. Table-view toggle (accessibility + fastest way to grab numbers); mobile pass; remove banner placeholder.

**Effort:** ~1 week spread across the others.

---

## Suggested sequence

Plan 1 → Plans 5 + 8 + 9.1 (quick wins, days) → Plans 3 + 4 (a week each) → Plan 2 (the big lift) → Plan 7 (parallel, mostly writing) → Plan 6 (last).
