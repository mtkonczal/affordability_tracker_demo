# Affordability Tracker v2 — Plan

*Drafted July 2026, synthesizing the comms team feedback, the Urban Institute
American Affordability Tracker technical appendix review, and a fresh-eyes
review of the current codebase.*

**Status (July 2026):** Phases 1, 2, and 4 are implemented, plus an "About
the Data" page (`about.html`) and a three-view architecture (National / My
State / Compare States) that makes within-state and across-state comparison
first-class. Phase 3 (rankings, report cards, choropleth) was descoped by
decision; Phase 5 (distribution toolkit) is deferred, though the copy-fact
button from Phase 1 shipped. Departures from the plan as written: state
rents required aggregating Zillow's county ZORI with ACS renter weights
(Zillow publishes no state file), and state electricity bills are wired up
but dormant until an `EIA_KEY` secret is added.

## Where we are

The foundation is genuinely good and worth keeping: an automated, fail-loud
FRED/BLS pipeline on a biweekly cron; ~50 series across daily items,
groceries, big-ticket, labor, debt, and a 5-state pilot; branded PNG and CSV
export on every card; a year-by-year heatmap; honest sourcing with FRED
links; and a zero-backend static deploy. None of that needs rebuilding.

Four things hold it back for the actual audience (campaigners and media):

1. **It's national, but the users are in states.** State data is a bolt-on
   category: 5 states, 4 series. Comms is explicit that state-level data is
   the #1 need. Urban's tracker wins on geography, and most of what it uses
   at the state level is free and public.
2. **It speaks in index points, not dollars.** Rebased CPI percent changes
   are correct but campaigners need "$X more per month." Urban's other big
   edge is dollar *levels* (rent in dollars, electricity bills in dollars),
   and several of those sources are free.
3. **The time anchors don't match the message.** The story is (a) since
   January 2025, (b) since before the pandemic, (c) over a generation. The
   current default is "since 2020," which bakes the COVID crash into the
   baseline (gas cratered in April 2020, so "since 2020" gas numbers are
   inflated in a way that won't survive scrutiny). There is no pre-pandemic
   anchor and no generation view.
4. **It shows data but doesn't hand people the sentence.** The gap between
   "here's a chart" and "STATE ranks 47th in rent burden; groceries are up
   21% since Trump took office" is the product. Rankings, two-state
   comparisons, copyable fact strings, and report cards are what turn a
   dashboard into a campaign tool.

The strategic reframe for v2: **from a chart gallery with a state tab to
"pick your state, get your talking points."**

---

## Phase 1 — Message-ready framing + UX fixes (national data we already have)

Fast, unblocked, all front-end plus small R changes.

- **Three standard anchors everywhere:** Jan 2025 · Dec 2019 (pre-pandemic)
  · 2000 (a generation). A single global toggle that drives cards, hero
  stats, heatmap, and PNG exports. Keep 1Y. Use **Dec 2019, not Jan 2020**,
  as the pre-pandemic anchor to avoid COVID base effects.
- **Dual change badges on every card:** show "since Jan 2025" and "since
  pre-pandemic" simultaneously rather than making users toggle. These are
  the two numbers every press release needs.
- **Copy-the-fact button:** one click copies "Grocery prices are up X% since
  January 2025 and Y% since before the pandemic (BLS data via ESP
  Affordability Tracker)." Nearly free to build; directly serves the
  press-release use case comms described.
- **Comms UX asks, all small:**
  - Additive filtering: category pills add to the selection instead of
    resetting it; unselected series stay visible as grayed chips so you can
    mix "Big Ticket + groceries" without re-clicking everything.
  - Color families per category (groceries in greens, big-ticket in
    purples, etc.) so multi-series views scan quickly.
  - One decimal place on percent labels throughout.
  - $ vs % display toggle wherever dollar units exist.
- **Heatmap:** keep it (comms loves it); add cumulative "since 2019" and
  "since Jan 2025" columns alongside the year-by-year columns.

## Phase 2 — State expansion to all 50 + DC (the core bet)

- **Scale the existing FRED state series from 5 states to 50 + DC.** The ID
  patterns are systematic (`{ST}UR`, `{ST}STHPI`, `MEHOINUS{ST}A672N`,
  `SMU{fips}000000500000003`). Add state average weekly earnings (same CES
  survey Urban uses, monthly, on FRED). Write a scripted pre-flight that
  verifies every generated ID against `fredgraph.csv` before it enters
  `SERIES`, and add retry/backoff (or a free FRED API key) since this is
  ~250 requests per refresh.
- **Add the dollar-level state sources Urban proved out — all free:**
  | Source | What it gives | Geography | Frequency |
  |---|---|---|---|
  | Zillow ZORI | Market rent, $/mo | All states + metros, one CSV | Monthly |
  | EIA-861M (EIA API) | Avg residential electricity bill, $ | All states | Monthly |
  | FHFA HPI | Home price index | All states (already have 5) | Quarterly |
  | EIA gas prices | $/gallon | ~9 states + PADD regions | Weekly |
- **Split the data payload.** Keep `data/app_data.js` national; write
  `data/states/{st}.js` and lazy-load when a state is picked. At current
  density, 50 states in one file would be several MB; the split keeps first
  paint at today's ~515K or less.
- **Deep links:** `#state=IL&since=2025` so an organizer can send a
  colleague straight to their state and framing.

## Phase 3 — Rankings, comparisons, scorecards (the proof-point layer)

This is what makes it a campaign tool rather than a reference site.

- **Build-time rankings in R:** for every state metric, compute latest
  value, change since each anchor, national rank, and gap vs. the US
  average; write `rankings.json`. Enables "STATE ranks 47th" with zero
  client-side work.
- **Rent burden:** ACS table B25070 (share of renters paying 30%+ of
  income), all states, via the Census API (`CENSUS_API_KEY` already in
  `.Renviron`). Annual, but it's the canonical "Nth worst rent burden"
  stat.
- **Hours-of-work per state:** state average hourly wage vs. ZORI rent and
  the grocery basket — "a month's rent costs N hours of work in Ohio." This
  is the comms stopwatch idea, made rigorous and automatic.
- **Two-state comparison view:** "Illinois vs. Wisconsin" side-by-side with
  an auto-written difference sentence ("Illinois families pay $X more per
  month in rent than Wisconsin families").
- **Choropleth map:** change since Jan 2025 by state, per metric. The
  natural extension of the heatmap comms already likes.
- **State report card export:** a templated one-page PNG/PDF per state
  (reusing the existing canvas export code): six headline stats, rank
  badges, both anchors, ESP branding. Comms asked for exactly this.

## Phase 4 — Annual depth: the big-ticket state gaps + debt

These update annually, so they serve rankings and report cards rather than
live tracking. Label them clearly as annual so the "live" promise isn't
diluted.

- **KFF ACA benchmark premiums** by state (annual, public).
- **Child Care Aware of America** childcare prices by state (annual PDFs;
  compile once a year into a hand-checked CSV committed to the repo).
- **NY Fed State-Level Household Debt Statistics** — balances and
  delinquency per capita by state (annual XLSX, public). This is the
  closest free substitute for Urban's proprietary credit panel.
- **Urban "Debt in America" medical debt in collections** by state
  (publicly downloadable from Urban's data catalog, with attribution) —
  directly enables the "3rd highest medical debt" line comms wants.

## Phase 5 — Distribution toolkit (build with comms)

- **"What moved this month" box + `changes.json`:** each data refresh
  auto-generates the biggest movers vs. prior month and vs. anchors. This is
  the rapid-response feed for the biweekly content package.
- **Data release calendar:** next CPI, jobs day, G.19 dates, so media know
  when numbers refresh.
- **Embeddable single-card iframes** (`?embed=groceries&state=oh`) for
  partner and campaign sites.
- **Social copy templates** with live numbers interpolated, and a batch
  "download all cards for my state" export to front-load infographics
  before launch.

---

## Trade-offs and honest limits

- **No state CPI exists.** State-level grocery/insurance price *levels* are
  the one piece we cannot replicate without proprietary data (NielsenIQ,
  ICE, Urban's credit panel). The defensible middle ground: BLS publishes
  CPI for 4 census regions and ~23 large metros, including food at home, so
  we can say "grocery inflation in the Midwest." The tracker already
  discloses that no state CPI exists; keep that discipline. Rigor is the
  moat — every number must survive a hostile fact-check.
- **Skip congressional districts for v2.** It's Urban's other edge, but the
  crosswalk work is heavy and the underlying data thin. States are the
  80/20 for activists.
- **Failure policy needs to evolve.** Today any single failed series blocks
  the entire data commit (correct for 50 series, one source). At 250+
  series across FRED, BLS, Zillow, EIA, and Census, one flaky source
  blocking all updates becomes the common case. Proposal: per-source
  isolation — commit what succeeded, mark affected cards stale in the UI,
  still alert loudly. Decide before Phase 2 lands.
- **Front-end performance.** The page ships React *development* builds and
  compiles JSX in the browser via Babel standalone on every load. Fine for
  a demo; slow on mobile for a launch destination. Minimum fix: production
  React builds + a one-time JSX precompile (still no bundler or framework
  workflow). Decide before launch.

## Suggested sequencing

| Phase | Depends on | Rough effort |
|---|---|---|
| 1. Anchors, fact-copy, UX fixes | Nothing | Days |
| 2. 50-state expansion + new sources | ID pre-flight, payload split | ~1 week incl. verification |
| 3. Rankings, comparisons, report cards | Phase 2 | Biggest design lift |
| 4. Annual layers (KFF, CCAoA, NY Fed, medical debt) | Phase 3 UI | One sprint, then yearly maintenance |
| 5. Toolkit + distribution | Phases 1–3 | Ongoing, with comms |

Phases 1 and the Phase 2 pre-flight are unblocked today and can run in
parallel.
