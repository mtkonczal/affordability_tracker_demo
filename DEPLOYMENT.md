# Deployment

The tracker is a static site (`index.html` + `data/`) with no backend. It's
meant to run on GitHub Pages and refresh itself on a schedule; ESP's site
just embeds it via iframe. This doc covers the one-time setup and how the
pieces fit together.

## One-time setup (GitHub UI — requires repo admin)

1. **Add the `BLS_KEY` secret.** Settings → Secrets and variables → Actions →
   New repository secret → name `BLS_KEY`, value your BLS API key. Several
   series (car insurance, health insurance, childcare, and the meat/dairy/
   produce items) come from the BLS API rather than FRED's public CSV
   endpoint and need this key to fetch.
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
- Runs on the 2nd and 16th of each month (a day or two after most CPI
  releases), and on-demand via the "Run workflow" button in the Actions tab.
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

```html
<iframe
  src="https://<your-pages-domain>/"
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
- `min-height` is a rough starting point; the page's real height varies with
  how many cards are selected. If ESP's CMS supports it, a small
  `postMessage`-based auto-resize script is a reasonable follow-up — not
  built here since it'd need coordination with however the CMS embeds
  iframes.

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
