# Deployment

The tracker is a static site (`index.html` + `data/`) with no backend. It's
meant to run on GitHub Pages and refresh itself on a schedule; ESP's site
just embeds it via iframe. This doc covers the one-time setup and how the
pieces fit together.

## One-time setup (GitHub UI — requires repo admin)

1. **Add the API-key secrets.** Settings → Secrets and variables → Actions →
   New repository secret:
   - `BLS_KEY` (required) — several series (car insurance, health insurance,
     childcare, and the meat/dairy/produce items) come from the BLS API
     rather than FRED's public CSV endpoint.
   - `CENSUS_API_KEY` (required) — county renter-household weights for the
     state rent aggregation. Free signup:
     https://api.census.gov/data/key_signup.html
   - `EIA_KEY` (optional) — enables the state electricity-bill metric. Free
     signup: https://www.eia.gov/opendata/. Without it the script skips that
     metric with a loud message and everything else still refreshes.
2. **Enable GitHub Pages.** Settings → Pages → Source: "Deploy from a
   branch" → Branch: `main` / root. (If you'd rather not serve the raw repo
   root, this can be pointed at a dedicated branch instead — ask if you want
   that instead of the default.)
3. **Optional: custom domain.** Settings → Pages → Custom domain, e.g.
   `affordability.economicsecurityproject.org`, plus a CNAME record at your
   DNS provider pointing at `<org>.github.io`.
4. **Check failure notifications.** GitHub emails repo watchers by default
   when a scheduled Action run fails — confirm your notification settings
   (github.com → Settings → Notifications) include Actions failures.

## What runs automatically

`.github/workflows/update-data.yml`:
- Runs on the 1st of each month, and on-demand via the "Run workflow"
  button in the Actions tab. (CPI releases mid-month; add a second cron
  date if you want the new print picked up within a day or two.)
- Installs R, runs `Rscript fetch_data.R`, and commits `data/` if anything
  changed.
- Permissions are `contents: write` only — nothing else in the repo or
  organization is touched.
- **If any single series fails to fetch** (bad ID, source outage),
  `fetch_data.R` exits with a non-zero status *after* trying every other
  series, so the commit step is skipped entirely. The site keeps serving
  whatever was last committed — stale, never broken — and the failed run
  shows up in the Actions tab (and in your notification email).

## Embedding on the ESP site

Add `?embed=1` to the URL for the embed variant: the navy topper is dropped
(the ESP site header takes its place), the view tabs move into a light menu
bar, the menu bars render `#F4F2E4`, and the chart area goes white. Without
the parameter the page keeps its standalone design.

```html
<iframe
  id="affordability-tracker"
  src="https://<your-pages-domain>/?embed=1"
  title="Affordability Tracker"
  style="width:100%; border:0; min-height:1400px;"
  sandbox="allow-scripts allow-popups allow-downloads"
  loading="lazy">
</iframe>
```

- `sandbox` deliberately omits `allow-same-origin`, so the iframe's content
  runs as an opaque, cookie-less origin even though it needs `allow-scripts`
  for React/Chart.js. The page has no forms, no login, and no user data, so
  this costs nothing — but it means if the tracker's own code were ever
  compromised upstream, it couldn't read or write anything on ESP's origin.
- **Auto-resize:** the page posts its content height to the parent window
  whenever it changes (view switches, cards toggled, window resized):
  `{ type: "esp-dashboard-height", height: <px> }`. The ESP page listens and
  sets the iframe height, e.g.:

```html
<script>
  window.addEventListener("message", function (e) {
    if (e.data && e.data.type === "esp-dashboard-height") {
      document.getElementById("affordability-tracker").style.height =
        e.data.height + "px";
    }
  });
</script>
```

  `min-height` on the iframe is just the pre-resize fallback.

## Manual refresh (no waiting for the schedule)

```bash
Rscript fetch_data.R
git add data/
git commit -m "Manual data refresh"
git push
```

Or trigger the Action by hand: Actions tab → "Update affordability data" →
Run workflow.

## Local preview

```bash
open index.html
```

No build step, no server required — it's a single self-contained HTML file
that reads `data/app_data.js`.
