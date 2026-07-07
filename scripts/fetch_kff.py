#!/usr/bin/env python3
"""Download KFF State Health Facts indicators as multi-year time series.

Two indicators, both national + all 50 states + DC, for every year KFF
publishes:

  - ACA benchmark premium   (second-lowest-cost silver, 40-year-old)
      https://www.kff.org/affordable-care-act/state-indicator/average-marketplace-premiums-by-metal-tier/
  - Uninsured rate          (share of the total population with no coverage)
      https://www.kff.org/state-health-policy-data/state-indicator/total-population/

How the download works
----------------------
KFF has no public CSV/API for the full multi-year table. Each indicator *is*
backed by a Google Sheet (the page carries a `gdocs_key`), but the Sheets CSV
export returns only one tab (one year) at a time. The complete table for every
year is embedded in the indicator page's HTML as a `gdocsObject` JSON blob:

    [ ["2026", [ [header row], [format row], [state rows...] ]],
      ["2025", [ ... ]], ... ]

We fetch the page, pull out that blob, and select one column by name (so a
column reorder upstream doesn't silently grab the wrong metric). This is the
same "extract the embedded JSON" approach the annual layers already use; it
needs no API key.

Outputs (all committed artifacts; re-run yearly, don't hand-edit)
----------------------------------------------------------------
  data/kff/aca_benchmark_premium.csv   long: date,code,value   ($ per month)
  data/kff/uninsured_rate.csv          long: date,code,value   (percent)
      date = YYYY-01-01, code = "US" or 2-letter postal.
      fetch_data.R reads these into the national series + state metrics.

  data/annual/raw/kff_metal_tier_premiums.csv   full provenance (all 4 tiers)
  data/annual/raw/kff_coverage.csv              full provenance (all coverage %)

Run from the repo root:  python3 scripts/fetch_kff.py
"""

import csv
import json
import os
import sys
import urllib.request

KFF_DIR = os.path.join("data", "kff")
RAW_DIR = os.path.join("data", "annual", "raw")

# KFF gives full state names; the tracker keys everything by postal code.
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


def location_code(name):
    """'United States' -> 'US'; a state name -> postal; anything else -> None
    (drops KFF's Puerto Rico / territory / footnote rows)."""
    name = (name or "").strip()
    if name in ("United States", "United States of America"):
        return "US"
    return STATE_NAMES.get(name)


def fetch_page(url):
    req = urllib.request.Request(url, headers={
        # KFF serves the page to a default UA fine; a browser-ish UA is just
        # belt-and-suspenders against bot filtering.
        "User-Agent": "Mozilla/5.0 (affordability-tracker KFF fetcher; contact repo owner)"
    })
    with urllib.request.urlopen(req, timeout=60) as r:
        return r.read().decode("utf-8", "replace")


def _match_array(html, start):
    """Given the index of an opening '[', return the balanced JSON array text,
    respecting quoted strings and escapes."""
    depth, in_str, esc = 0, False, False
    for j in range(start, len(html)):
        c = html[j]
        if in_str:
            if esc:
                esc = False
            elif c == "\\":
                esc = True
            elif c == '"':
                in_str = False
        else:
            if c == '"':
                in_str = True
            elif c == "[":
                depth += 1
            elif c == "]":
                depth -= 1
                if depth == 0:
                    return html[start:j + 1]
    raise ValueError("gdocsObject array never closed")


def extract_gdocs_object(html):
    """Pull the year-keyed `gdocsObject` data table out of the page HTML.

    The page carries more than one `gdocsObject` (one is a settings/metadata
    table). Parse every occurrence and return the one whose blocks are keyed by
    a 4-digit year with a header/format/data shape."""
    marker = '"gdocsObject":'
    pos = 0
    candidates = []
    while True:
        i = html.find(marker, pos)
        if i == -1:
            break
        start = html.find("[", i)
        if start == -1:
            break
        text = _match_array(html, start)
        pos = start + len(text)
        try:
            obj = json.loads(text)
        except json.JSONDecodeError:
            continue
        candidates.append(obj)

    if not candidates:
        raise ValueError("gdocsObject not found in page (KFF may have changed layout)")

    def is_year_table(obj):
        return (isinstance(obj, list) and obj
                and isinstance(obj[0], list) and len(obj[0]) == 2
                and isinstance(obj[0][0], str) and obj[0][0].strip().isdigit()
                and len(obj[0][0].strip()) == 4
                and isinstance(obj[0][1], list) and len(obj[0][1]) >= 3)

    for obj in candidates:
        if is_year_table(obj):
            return obj
    raise ValueError("no year-keyed gdocsObject data table found "
                     f"among {len(candidates)} candidate(s)")


def column_index(header, fmt_row, want_label, want_format=None):
    """Find the column whose header == want_label (and, if given, whose format
    row cell == want_format, to disambiguate the repeated Percent/Number blocks
    in the coverage table). Column 0 is the location label, so we search from 1.
    Returns the FIRST match."""
    for k in range(1, len(header)):
        if str(header[k]).strip() != want_label:
            continue
        if want_format is not None:
            cell = fmt_row[k] if k < len(fmt_row) else ""
            if str(cell).strip() != want_format:
                continue
        return k
    raise ValueError(f"column {want_label!r} "
                     f"(format {want_format!r}) not found; header={header}")


def parse_indicator(gdocs, want_label, want_format, transform):
    """Walk every year block, select one column by name, and return:
      series[code] = {year(int): value(float)}   for US + states we track
      raw_rows     = list of (year, location, [all data cells])  for provenance
      raw_header   = the header row (from the newest year block)
    """
    series = {}
    raw_rows = []
    raw_header = None
    for year_block in gdocs:
        year = str(year_block[0]).strip()
        # KFF appends a trailing "Notes" block (source/footnote metadata) after
        # the year blocks; skip anything that isn't a 4-digit year.
        if not (year.isdigit() and len(year) == 4):
            continue
        table = year_block[1]
        header, fmt_row, data_rows = table[0], table[1], table[2:]
        if raw_header is None:
            raw_header = header
        col = column_index(header, fmt_row, want_label, want_format)
        for row in data_rows:
            if not row:
                continue
            loc = str(row[0]).strip()
            code = location_code(loc)
            raw_rows.append((year, loc, row))
            if code is None:
                continue
            cell = row[col] if col < len(row) else ""
            cell = str(cell).strip().replace(",", "").replace("$", "")
            if cell in ("", "N/A", "NA", "NR", "--", "*"):
                continue
            try:
                val = transform(float(cell))
            except ValueError:
                continue
            series.setdefault(code, {})[int(year)] = val
    return series, raw_rows, raw_header


def write_long_csv(path, series, round_digits):
    """series[code] = {year: value} -> long CSV date,code,value sorted by
    (code, date). date is Jan 1 of the year so it lands on the annual grid the
    rest of the tracker uses (see MEHOINUS* income series)."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["date", "code", "value"])
        for code in sorted(series):
            for year in sorted(series[code]):
                v = round(series[code][year], round_digits)
                if round_digits == 0:
                    v = int(v)
                w.writerow([f"{year}-01-01", code, v])
    n_codes = len(series)
    years = sorted({y for d in series.values() for y in d})
    span = f"{years[0]}–{years[-1]}" if years else "no years"
    print(f"  wrote {path}: {n_codes} geographies, {span}")


def write_raw_csv(path, header_cells, raw_rows):
    """Full provenance dump: every location KFF lists, every year, all columns."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    cols = ["year", "location"] + [str(c).strip() for c in header_cells[1:]]
    with open(path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(cols)
        for year, loc, row in raw_rows:
            w.writerow([year, loc] + [str(c).strip() for c in row[1:]])
    print(f"  wrote {path}: {len(raw_rows)} rows (raw provenance)")


INDICATORS = [
    {
        "id": "aca_benchmark_premium",
        "url": "https://www.kff.org/affordable-care-act/state-indicator/"
               "average-marketplace-premiums-by-metal-tier/",
        "column": "Average Benchmark Premium",
        "column_format": None,
        # KFF publishes dollars already; keep as-is, whole dollars.
        "transform": lambda v: v,
        "round": 0,
        "raw_file": "kff_metal_tier_premiums.csv",
    },
    {
        "id": "uninsured_rate",
        "url": "https://www.kff.org/state-health-policy-data/state-indicator/"
               "total-population/",
        "column": "Uninsured",
        "column_format": "Percent",          # not the "Number" block
        # KFF stores shares as fractions (0.08202); publish as a percent.
        "transform": lambda v: v * 100,
        "round": 1,
        "raw_file": "kff_coverage.csv",
    },
]


def main():
    if not os.path.isdir("data"):
        sys.exit("Run from the repo root (no ./data directory here).")
    print("Fetching KFF indicators:")
    failures = []
    for ind in INDICATORS:
        print(f"\n{ind['id']}  <-  {ind['url']}")
        try:
            html = fetch_page(ind["url"])
            gdocs = extract_gdocs_object(html)
            series, raw_rows, header = parse_indicator(
                gdocs, ind["column"], ind["column_format"], ind["transform"])
            if "US" not in series:
                raise ValueError("no United States row parsed")
            missing = set(STATE_NAMES.values()) - set(series)
            if missing:
                print(f"  note: no data for {', '.join(sorted(missing))}")
            write_long_csv(os.path.join(KFF_DIR, ind["id"] + ".csv"),
                           series, ind["round"])
            write_raw_csv(os.path.join(RAW_DIR, ind["raw_file"]), header, raw_rows)
        except Exception as e:  # noqa: BLE001 - report and keep going
            print(f"  ✗ FAILED: {e}")
            failures.append(ind["id"])

    if failures:
        sys.exit(f"\n✗ {len(failures)} indicator(s) failed: {', '.join(failures)}")
    print("\nDone. Re-run `Rscript fetch_data.R` to fold these into "
          "data/app_data.js and the state payloads.")


if __name__ == "__main__":
    main()
