#!/usr/bin/env python3
"""Convert raw annual state datasets into the tracker's data/annual/*.csv format.

Reads the raw source files committed under data/annual/raw/ and writes one
CSV per indicator with columns: state,value (state = 2-letter postal code,
50 states + DC). fetch_data.R attaches anything listed in
data/annual/annual_meta.json to the state payloads.

Raw source provenance (see data/annual/README.md for full details):
  - area_report_by_year.xlsx          NY Fed State-Level Household Debt Statistics
  - urban_state_national_medical.csv  Urban Institute Debt in America (Oct 2025 update)
  - urban_state_national_overall.csv  Urban Institute Debt in America (Oct 2025 update)
  - ccaoa_2025_center_infant.csv      Child Care Aware of America 2025 (extracted)

Note: KFF ACA premiums and the uninsured rate are handled separately, as full
time series, by scripts/fetch_kff.py (they feed national + state metrics, not
the single-value annual tiles here).

This script also writes the NY Fed *time series* CSVs into data/nyfed/
(long format: date,code,value — same shape as data/kff/). The NY Fed area
report carries every Q4 back to 2003 plus an allUS row, so household debt
per capita, student-loan debt per capita, and 90+ day credit-card
delinquency feed full annual national cards + state metrics (source =
"nyfed" in fetch_data.R), not single-value tiles.

Run from the repo root:  python3 scripts/convert_annual.py
"""

import csv
import os
import sys

RAW = os.path.join("data", "annual", "raw")
OUT = os.path.join("data", "annual")

STATE_NAMES = {
    "Alabama": "AL", "Alaska": "AK", "Arizona": "AZ", "Arkansas": "AR",
    "California": "CA", "Colorado": "CO", "Connecticut": "CT", "Delaware": "DE",
    "District of Columbia": "DC", "Florida": "FL", "Georgia": "GA", "Hawaii": "HI",
    "Idaho": "ID", "Illinois": "IL", "Indiana": "IN", "Iowa": "IA", "Kansas": "KS",
    "Kentucky": "KY", "Louisiana": "LA", "Maine": "ME", "Maryland": "MD",
    "Massachusetts": "MA", "Michigan": "MI", "Minnesota": "MN", "Mississippi": "MS",
    "Missouri": "MO", "Montana": "MT", "Nebraska": "NE", "Nevada": "NV",
    "New Hampshire": "NH", "New Jersey": "NJ", "New Mexico": "NM", "New York": "NY",
    "North Carolina": "NC", "North Dakota": "ND", "Ohio": "OH", "Oklahoma": "OK",
    "Oregon": "OR", "Pennsylvania": "PA", "Rhode Island": "RI", "South Carolina": "SC",
    "South Dakota": "SD", "Tennessee": "TN", "Texas": "TX", "Utah": "UT",
    "Vermont": "VT", "Virginia": "VA", "Washington": "WA", "West Virginia": "WV",
    "Wisconsin": "WI", "Wyoming": "WY",
}
VALID = set(STATE_NAMES.values())


def write_out(name, rows):
    """rows: list of (state, value). Validates coverage and writes CSV."""
    rows = [(s, v) for s, v in rows if s in VALID]
    codes = {s for s, _ in rows}
    missing = VALID - codes
    path = os.path.join(OUT, name)
    with open(path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["state", "value"])
        for s, v in sorted(rows):
            w.writerow([s, v])
    note = f" (missing: {', '.join(sorted(missing))})" if missing else ""
    print(f"  {name}: {len(rows)} states{note}")


def convert_urban():
    for src, col, out in [
        ("urban_state_national_medical.csv", "medcoll", "medical_debt.csv"),
        ("urban_state_national_overall.csv", "totcoll", "debt_collections.csv"),
    ]:
        with open(os.path.join(RAW, src)) as f:
            rows = []
            for r in csv.DictReader(f):
                abbr, val = r.get("abbr"), r.get(col)
                if abbr in VALID and val not in (None, "", "NA"):
                    v = float(val)
                    # Urban encodes suppressed states as 0 — in the 2025
                    # medical file these are the seven states (CA, CO, IL,
                    # NY, RI, VT, WA) that restrict medical-debt credit
                    # reporting. A literal 0.0% share is implausible; treat
                    # zeros as missing rather than publish them.
                    if v <= 0:
                        continue
                    # Shares arrive as fractions (0.0587) — publish as percent
                    rows.append((abbr, round(v * 100, 1)))
        write_out(out, rows)


def convert_ccaoa():
    with open(os.path.join(RAW, "ccaoa_2025_center_infant.csv")) as f:
        rows = []
        for r in csv.DictReader(f):
            # Seven states carry a "*" footnote flag in CCAoA's published
            # tables (meaning not machine-retrievable; see README caveat).
            # We publish CCAoA's values as printed, flag stripped.
            name = r["State"].replace("*", "").strip()
            price = r["Infant Price ($)"].strip()
            # A few states (CO, DC, NM, SC in 2025) have no reported price —
            # they simply won't get this tile.
            if name in STATE_NAMES and price:
                rows.append((STATE_NAMES[name], int(float(price))))
    write_out("childcare_infant.csv", rows)


def convert_nyfed():
    """Write NY Fed annual time series (all Q4s, 2003–present) to data/nyfed/.

    Long format matching data/kff/: date,code,value. Q4 observations are
    dated YYYY-12-01 so the tracker's Dec-2019 pre-pandemic anchor lands
    exactly on the Q4 2019 point. The sheet's 'allUS' row becomes code US,
    giving the state metrics a national overlay from the same credit panel
    (source consistency — no mixing of NY Fed states with G.19 national).
    """
    try:
        import openpyxl
    except ImportError:
        sys.exit("openpyxl required: pip install openpyxl")
    wb = openpyxl.load_workbook(os.path.join(RAW, "area_report_by_year.xlsx"), read_only=True)

    def sheet_series(sheet_name):
        """-> (rows, latest_label): rows = [(date, code, value)] for all Q4s."""
        ws = wb[sheet_name]
        grid = [[c for c in row] for row in ws.iter_rows(values_only=True)]
        # Find the header row ("state", "Q4_2003", ...)
        hdr_i = next(i for i, row in enumerate(grid)
                     if row and str(row[0]).strip().lower() == "state")
        header = [str(c).strip() if c is not None else "" for c in grid[hdr_i]]
        q_cols = [(j, h) for j, h in enumerate(header) if h.startswith("Q4_")]
        rows = []
        for row in grid[hdr_i + 1:]:
            if not row or row[0] is None:
                continue
            code = str(row[0]).strip()
            code = "US" if code == "allUS" else code.upper()
            if code != "US" and code not in VALID:
                continue
            for j, h in q_cols:
                if row[j] is None:
                    continue
                rows.append((f"{h[3:]}-12-01", code, float(row[j])))
        return rows, max(h for _, h in q_cols)

    out_dir = os.path.join("data", "nyfed")
    os.makedirs(out_dir, exist_ok=True)

    def write_series(name, rows, fmt):
        path = os.path.join(out_dir, name)
        with open(path, "w", newline="") as f:
            w = csv.writer(f)
            w.writerow(["date", "code", "value"])
            for date, code, v in sorted(rows, key=lambda r: (r[1], r[0])):
                w.writerow([date, code, fmt(v)])
        n_codes = len({c for _, c, _ in rows})
        n_years = len({d for d, _, _ in rows})
        print(f"  nyfed/{name}: {n_codes} geographies x {n_years} years")

    for sheet, name, is_dollar in [
        ("total",             "debt_per_capita.csv",        True),
        ("studentloan",       "studentloan_per_capita.csv", True),
        ("creditcard_delinq", "cc_delinquency_90.csv",      False),
    ]:
        rows, label = sheet_series(sheet)
        if is_dollar:
            write_series(name, rows, lambda v: int(round(v)))
        else:
            # Percent of balance 90+ days delinquent. Guard against a
            # fraction-vs-percent format change between releases.
            sample = [v for _, _, v in rows[:40]]
            scale = 100 if max(sample) < 1 else 1
            write_series(name, rows, lambda v: round(v * scale, 1))
    print(f"  NY Fed vintage: {label}")


if __name__ == "__main__":
    if not os.path.isdir(RAW):
        sys.exit(f"Raw source directory not found: {RAW}")
    print("Converting annual state datasets:")
    convert_urban()
    convert_ccaoa()
    convert_nyfed()
    print("Done. Re-run `Rscript fetch_data.R` to embed into state payloads.")
