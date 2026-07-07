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
  - kff_benchmark_premiums.csv        KFF Marketplace Average Benchmark Premiums (extracted)
  - ccaoa_2025_center_infant.csv      Child Care Aware of America 2025 (extracted)

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


def convert_kff(year="2026"):
    with open(os.path.join(RAW, "kff_benchmark_premiums.csv")) as f:
        rows = []
        for r in csv.DictReader(f):
            if r["year"] == year and r["location"] in STATE_NAMES:
                rows.append((STATE_NAMES[r["location"]], int(float(r["average_benchmark_premium"]))))
    write_out("aca_benchmark.csv", rows)


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
    try:
        import openpyxl
    except ImportError:
        sys.exit("openpyxl required: pip install openpyxl")
    wb = openpyxl.load_workbook(os.path.join(RAW, "area_report_by_year.xlsx"), read_only=True)

    def sheet_latest(sheet_name):
        ws = wb[sheet_name]
        grid = [[c for c in row] for row in ws.iter_rows(values_only=True)]
        # Find the header row ("state", "Q4_2003", ...)
        hdr_i = next(i for i, row in enumerate(grid)
                     if row and str(row[0]).strip().lower() == "state")
        header = [str(c).strip() if c is not None else "" for c in grid[hdr_i]]
        q_cols = [j for j, h in enumerate(header) if h.startswith("Q4_")]
        latest_j = max(q_cols, key=lambda j: header[j])
        latest_label = header[latest_j]
        rows = []
        for row in grid[hdr_i + 1:]:
            if not row or row[0] is None:
                continue
            code = str(row[0]).strip().upper()
            val = row[latest_j]
            if code in VALID and val is not None:
                rows.append((code, val))
        return rows, latest_label

    total, label_t = sheet_latest("total")
    write_out("nyfed_total_debt.csv", [(s, int(round(float(v)))) for s, v in total])

    cc, label_c = sheet_latest("creditcard_delinq")
    # Sheet values are percent of balance 90+ days delinquent. Guard against
    # a fraction-vs-percent format change between releases.
    sample = [float(v) for _, v in cc[:10]]
    scale = 100 if max(sample) < 1 else 1
    write_out("nyfed_cc_delinquency.csv", [(s, round(float(v) * scale, 1)) for s, v in cc])
    print(f"  NY Fed vintage: {label_t} / {label_c}")


if __name__ == "__main__":
    if not os.path.isdir(RAW):
        sys.exit(f"Raw source directory not found: {RAW}")
    print("Converting annual state datasets:")
    convert_urban()
    convert_kff()
    convert_ccaoa()
    convert_nyfed()
    print("Done. Re-run `Rscript fetch_data.R` to embed into state payloads.")
