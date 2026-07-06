# Fable Log — Affordability Tracker

Conversation log for the tracker planning session. Maintained per `tracker_next_steps.md`.

---

## Session: July 6, 2026

### User

> Execute tracker_next_steps.md.

Contents of `tracker_next_steps.md`:

> You are a computer programmer assisting an affordability policy communications shop, and you are here to help build out a draft affordability tracker to explain how things have gotten less affordable in the past 18 months, past 7 years, and past generation. Inside this folder is index.html which is the first pass using national, government data.
>
> Inside this folder is .R files for executing getting current and new data into a data folder, and feedback_from_comms.md.
>
> You have four steps. I want you to create a Fable_log.md that outputs the entire conversation we have, including all your responses that you'll display here, to track progress.
>
> The first is to optimize this so that we can drop it into our webpage and have it automatically update itself. [...] Security is essential here. Describe your plan.
>
> The second is to come up with a plan and list of operations from the feedback_from_comms.md document that we could potentially do. [...] Do not execute them. Just come up with the plan and your best suggestions.
>
> The third is to use your own judgment based on what is available and what could be useful for people in the field, reporters, and other communications and campaigns professionals. What would you suggest we do to make this more useful? Include in that your own thoughts about the design of the website itself. What could be better? What's working and what's not?

### Claude — actions taken

Read `tracker_next_steps.md`, `feedback_from_comms.md`, `fetch_data.R`, and `index.html` (the current React/Babel-in-browser variant). Ran two web searches to verify data-availability facts: the 2025 BLS CPI collection reductions (Lincoln NE, Provo UT, Buffalo NY suspended; ~15% of the remaining sample cut, raising volatility of subnational indexes) and the NY Fed Q1 2026 Household Debt and Credit release (state-level Consumer Credit Panel data; record credit card delinquency). Created this log. No code was changed — steps 1–3 are plans, as instructed.

### Claude — response

## Step 1 — Getting it on the website, self-updating, secure

**Recommendation: GitHub Pages + a scheduled GitHub Action, embedded on the ESP site via iframe.** The repo is already on GitHub and the site is already fully static, so this is the shortest path and also the most secure one.

How it works:

1. The site stays static — no server, no database, no accounts, no forms. `index.html` + `data/app_data.js`, exactly as now.
2. A GitHub Actions workflow runs on a schedule (e.g., the mornings after CPI release days, roughly the 2nd and 16th of each month): it installs R, runs `Rscript fetch_data.R`, and commits any changed `data/` files. The `BLS_KEY` lives as an encrypted Actions secret — it never appears in the site or the repo.
3. GitHub Pages serves the result over HTTPS. Optionally point a subdomain (e.g., `affordability.economicsecurityproject.org`) at it via CNAME.
4. The ESP website embeds it with a single sandboxed `<iframe>` — one line in the CMS, and none of the tracker's code ever runs on ESP's infrastructure.

Why this is secure: the attack surface is close to zero. There is nothing to hack into — no backend, no user data, no cookies, no inputs. The only external data sources are `fred.stlouisfed.org` and `api.bls.gov`, read at build time by the bot, not by visitors. The workflow gets least-privilege permissions (contents: write only).

Hardening to do before launch:

- **Replace the React dev builds + in-browser Babel.** The page currently loads `react.development.js`, `react-dom.development.js`, and Babel standalone from unpkg and compiles JSX in every visitor's browser. Precompile the JSX once (or fold the page back to the plain Chart.js architecture the repo documents) so the page ships zero compilers and no dev-mode code.
- **Vendor the JS libraries into the repo** (Chart.js and the date adapter currently load from jsDelivr without integrity hashes). Self-hosting removes every third-party runtime dependency; if CDNs must stay, add SRI `integrity` attributes.
- Add a `Content-Security-Policy` meta tag restricting scripts to self.
- Failure behavior is already right: if FRED/BLS is down, the Action fails and emails you, and the site keeps serving the last good data with its "Last updated" stamp. It degrades to *stale*, never to *broken*.

Maintenance: essentially none. Read the email if a run fails; once in a while a FRED series gets discontinued and needs a one-line ID swap.

Alternatives considered and rejected: Netlify/Cloudflare Pages (same model, adds a vendor for no gain); pasting the HTML into the CMS (updates become manual, fights the CMS); having visitors' browsers fetch FRED directly (CORS, rate limits, fragile).

## Step 2 — Plan from feedback_from_comms.md (not executed)

**A. State/local data (their top need).** The honest constraint first: *there is no state-level CPI.* BLS publishes CPI nationally, for 4 regions/9 divisions, and ~21 metro areas (many bi-monthly) — and 2025 budget cuts suspended collection in Lincoln, Provo, and Buffalo and trimmed ~15% of the remaining sample, making subnational indexes more volatile. So the plan is a mix:

- Metro CPI (all-items, food, shelter) for the published metros — covers the biggest media markets.
- Division-level CPI as the fallback so every state maps to *something*.
- True state-level series that do exist, nearly all on FRED already: unemployment (LAUS), average hourly earnings (state CES), median household income (ACS), FHFA house price index (quarterly), Zillow observed rent index (state + metro, monthly), EIA state electricity prices (monthly), EIA gas prices for selected states/regions (weekly).
- Annual state ranking data — perfect for the "STATE ranks 47th" use case and report cards: rent burden (ACS), medical debt in collections (Urban Institute *Debt in America*, state and county), credit card/auto/student delinquency (NY Fed Consumer Credit Panel state data — Q1 2026 showed record credit card delinquency, so this is potent), childcare prices (DOL National Database of Childcare Prices), health premiums (KFF/MEPS-IC).
- Architecture: add a `geo` field to `SERIES` in `fetch_data.R` plus a state picker in the UI. The fetch loop generalizes cleanly.

**B. Debt info.** NY Fed Household Debt & Credit quarterly (credit card, student, auto, delinquency rates, with state-level breakdowns) + Urban Institute for medical debt. Quarterly cadence satisfies "not static for multiple months."

**C. Real income / wage gap.** The hidden `labor` tab already has wages wired up. Plan: activate it, add inflation-adjusted wages, and build the two visuals comms sketched — the stacked-bills comparison (combined childcare + health + groceries + rent vs. median income) and the hours-of-work stopwatch. `index.html` already stubs `hoursToAfford()` and reference prices, so this was anticipated in the code.

**D. Trump prices.** A "Since 2025" range button already exists. Rename it "Since Jan 2025," add a scorecard row for change since inauguration, and consider making it the default range.

**E. Prices as % of income.** Computable now nationally; state-level once A lands (ACS income + state costs). Nice-to-have tier, as comms said.

**F. Sample content.** Because everything derives from data, templates can auto-fill with live numbers: press-release mad-libs, social copy strings, and — using the existing PNG export pipeline — batch-rendering 50 state report cards in the Actions workflow via headless Chrome, front-loading infographics before launch exactly as requested. AI prompt templates are a cheap add-on.

**G. Erion's UX items.** All cheap: color families by category, additive filters (unselected chips stay visible grayed-out; adding Groceries to Big Ticket no longer resets the selection), a %↔$ toggle (rebase helpers already exist), one-decimal percentages.

Suggested order: G + D (days) → C + B national (a week) → A metro/state (the real lift) → F (parallel; mostly writing) → E.

## Step 3 — My own judgment: usefulness and design

For reporters, field, and campaigns:

1. **A "copy this stat" button.** A reporter on deadline needs the sentence, not the chart: "Grocery prices are up 27.9% since January 2020 (BLS CPI via FRED, June 2026)." One click, citation included. Highest value-per-effort item on this whole list.
2. **Shareable URLs.** Selections and ranges currently vanish on reload, so a campaign staffer can't send a colleague the exact view they mean. Encode state in the URL hash.
3. **Per-chart embed snippets** so partner orgs and local outlets can drop a single ESP-branded chart into their own pages. That's how the tracker spreads beyond ESP's own site.
4. **A methodology page and a visible release calendar** ("Next update: July 15 — CPI day"). Credibility with reporters depends on being checkable, and the promise that data is current is stronger when you say when it updates.
5. **The most persuasive chart is missing: prices vs. wages on one canvas.** Every chart shows prices alone; the story is the gap. This should be the hero visual.
6. **Time updates to CPI mornings** and pair with the biweekly content package comms proposed — that's the moment reporters are searching for exactly this.

Design — what's working: the editorial newspaper look is distinctive and credible rather than generic-dashboard; per-card time ranges, CSV/PNG export, FRED source links on every card, the heatmap (which comms loved), and the last-updated stamp are all the right instincts.

What's not:

1. **The technical foundation needs consolidation before launch** — dev-build React plus in-browser Babel compilation makes first paint slow and diverges from the repo's own documented plain-Chart.js architecture (see Step 1).
2. **The two-level filter model is confusing**, and Erion's complaint is the symptom: pills pick a category, chips pick items, and switching pills wipes your selections. On first load the filter reads "All metrics" while only Daily items are actually checked. One additive multi-select fixes all of it.
3. **Red-means-up backfires on wages.** Rising hourly earnings render in red (bad) and falling in green. For the labor category the mapping must invert — wage growth is the good news in this story.
4. **Hardcoded copy will rot.** The hero subtext ("a basket that cost $100 then runs about $128") and the 2025 reference prices are frozen while the data updates around them. Compute them from the data.
5. **The heatmap measures January-over-January**, so by summer the current-year column is six months stale — use trailing 12 months for the latest column. And the red/green diverging scale is a problem for colorblind readers; the printed numbers rescue it, but red/blue is safer.
6. **Accessibility:** the charts have no text alternative. A "table view" toggle serves screen readers and doubles as the fastest way for a reporter to grab numbers.
7. **Mobile:** cards floor at 420px and the heatmap at 720px, forcing horizontal scrolling — much of the field audience will open this from a phone via a shared link. Worth a real pass.
8. The banner placeholder stripe, obviously, before anyone external sees it.

Sources: [BLS notice of CPI collection reductions](https://www.bls.gov/cpi/notices/2025/collection-reduction.htm) · [BLS: more information on CPI collection reductions](https://www.bls.gov/cpi/notices/2025/more-information-collection-reduction.htm) · [NY Fed Household Debt and Credit, Q1 2026](https://www.newyorkfed.org/newsevents/news/research/2026/20260512) · [NY Fed HHDC background (Consumer Credit Panel)](https://www.newyorkfed.org/microeconomics/hhdc/background.html)

---

### User

> Make a plan of things to do here, making each item in section 2 it's own numbered plan.

### Claude — response

Created `tracker_plan.md` with nine numbered plans: Plan 1 is deployment (GitHub Pages + Actions, front-end consolidation, CSP, iframe embed); Plans 2–8 give each Step 2 feedback item its own plan with concrete steps, effort, and dependencies — state/metro data (2), debt indicators (3), real income/wage gap (4), Trump prices (5), prices vs. income (6), sample content/infographics (7), Erion's UX fixes (8); Plan 9 collects the Step 3 site-quality and reporter-tool recommendations. Suggested sequence: Plan 1 → quick wins (5, 8, 9.1) → 3 + 4 → 2 (the big lift) → 7 in parallel → 6 last. See `tracker_plan.md` for the full text.

---

### User

> Execute 1-4, putting the new things you add into a new filter called "newest" - for version tracking purposes.

### Claude — actions taken

Probed the execution environment before changing code: the Cowork sandbox has Node 22 + npm but no R, and its network allowlist blocks FRED, BLS, jsDelivr, and unpkg from the shell (npm registry works; my web-fetch tool reaches FRED but returns the CSV unreadably). Conclusion: code/config can be fully wired here, but new-series data arrives via a local `Rscript fetch_data.R` run or the first GitHub Action run. Designed the "newest" filter as an `is_new: true` series flag + a `FILTERS` entry (keeps real categories intact). Asked about Plan 2 scope (pilot 5 states vs. metro-only vs. all 50).

### User

> Pause here and write out a next_steps_2.md file for where to go next.

### Claude — response

Wrote `next_steps_2.md`: environment constraints discovered, design decisions to carry forward (newest-filter mechanism; precompile-and-vendor front end; FRED-based debt series IDs; CPIAUCSL + derived real wages), the one open decision (Plan 2 state-data scope), and a six-step execution order for resuming. No repo code was modified — only planning/log markdown files.

---

*(Log will be appended as the conversation continues.)*
