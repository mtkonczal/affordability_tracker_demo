# Annual state layers

Slow-moving state-level context published once a year, shown as stat tiles on
each state's page. These are **not** part of the automated biweekly refresh —
they get updated once a year, by hand, when the sources publish.

## How it works

- `raw/` holds the original source files exactly as downloaded (provenance
  below).
- `scripts/convert_annual.py` (run from the repo root) converts `raw/` into
  one `state,value` CSV per indicator here.
- `annual_meta.json` describes each indicator (label, units, vintage, source
  link). `fetch_data.R` reads it and attaches any values it finds to the
  per-state payloads in `data/states/*.js`.
- Missing states are fine — the state page simply doesn't show that tile.

## Yearly refresh

1. Download new source files into `raw/` (URLs below).
2. `python3 scripts/convert_annual.py`
3. Update the `year` fields in `annual_meta.json`.
4. `Rscript fetch_data.R` to rebuild the state payloads.

## Sources and provenance (downloaded 2026-07-07)

> **ACA premiums and the uninsured rate moved out of this folder.** They are
> now full time series (national card + state metric), pulled by
> `scripts/fetch_kff.py` into `data/kff/*.csv` — not single-value tiles. See
> the project `CLAUDE.md`.

| Indicator | Source file | Origin |
|---|---|---|
| Infant child care price (2025) | `ccaoa_2025_center_infant.csv` | Child Care Aware of America, "Child Care in America: 2025 Price & Supply". Extracted from the report's Flourish table embeds (no machine-readable appendix exists). Annual center-based infant price. |
| Household debt per capita (Q4 2025) | `area_report_by_year.xlsx` | NY Fed State-Level Household Debt Statistics (Feb 2026 release), Consumer Credit Panel / Equifax. |
| Credit card delinquency 90+ (Q4 2025) | `area_report_by_year.xlsx` | Same file, `creditcard_delinq` sheet — percent of balance 90+ days delinquent. |
| Medical debt in collections (Aug 2025) | `urban_state_national_medical.csv` | Urban Institute "Debt in America" interactive map data (Oct 2025 update). Share of people with a credit record with medical debt in collections. |
| Any debt in collections (Aug 2025) | `urban_state_national_overall.csv` | Same, `totcoll` column. |

## Caveats (say these out loud before quoting)

- **CCAoA childcare:** CO, DC, NM, and SC have no reported 2025 infant price.
  Seven states (AL, FL, MT, PA, TX, WV, WY) carry a `*` footnote flag in
  CCAoA's published tables whose text isn't machine-retrievable — check the
  report PDF before building a campaign around those specific states.
- **Urban medical debt:** seven states that restrict medical-debt credit
  reporting (CA, CO, IL, NY, RI, VT, WA) appear in Urban's raw file with a
  literal 0 — that's suppression, not a 0.0% share. The converter drops
  zeros, so those states show no medical-debt tile. Technical appendix:
  https://apps.urban.org/features/debt-interactive-map/downloadable-docs/debt_interactive_technical_appendix_2025.pdf
- **NY Fed:** per-capita figures are per person *with a credit file* (ages
  18+), not per resident. Delinquency is percent of *balance*, not share of
  people.
- Attribution: Urban Institute data is theirs — cite "Urban Institute, Debt
  in America" wherever these numbers appear.
