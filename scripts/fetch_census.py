#!/usr/bin/env python3
"""Fetch annual Census ACS 1-year state series into data/census/*.csv.

Writes long CSVs (date,code,value — the same shape as data/kff/ and
data/nyfed/) that fetch_data.R folds into national cards + state metrics
via source = "census" entries in SERIES / STATE_METRICS.

Series produced:
  income_20th.csv  B19080_001E — household income, lowest-quintile upper
                   limit = the 20th percentile. Nominal dollars.
  income_80th.csv  B19080_004E — fourth-quintile upper limit = the 80th
                   percentile. Nominal dollars.
                   (The ACS publishes quintile limits, not arbitrary
                   percentiles; 20th/80th are the exact published points
                   closest to the requested 25th/75th. Interpolating
                   25th/75th would add distributional assumptions.)
  rent_burden.csv  B25070 — share of renter households paying 30%+ of
                   household income on gross rent:
                   (007+008+009+010) / (001 − 011 "not computed") × 100.

Geography: all states + DC + US. Years: 2006 onward (first year B19080 is
published), skipping 2020 (no standard 1-year release). Top-coding note: B19080 upper
limits are top-coded in some small states in early years; values arrive
as published.

Requires CENSUS_API_KEY in the environment or in .Renviron (the same key
fetch_data.R uses). Run yearly, from the repo root, after the September
ACS release:  python3 scripts/fetch_census.py
Then re-run fetch_data.R to embed into payloads.
"""

import csv
import json
import os
import re
import sys
import time
import urllib.request

# B19080 (income quintile upper limits) first appears in the ACS 1-year
# detailed tables in 2006 — 2005 returns "unknown variable". B25070 goes
# back further, but we start all three series together in 2006 for a
# consistent panel.
FIRST_YEAR = 2006
SKIP_YEARS = {2020}  # no standard 1-year ACS release


def get_key():
    key = os.environ.get("CENSUS_API_KEY", "")
    if not key and os.path.exists(".Renviron"):
        for line in open(".Renviron"):
            m = re.match(r"\s*CENSUS_API_KEY\s*=\s*['\"]?([^'\"\s]+)", line)
            if m:
                key = m.group(1)
    if not key:
        sys.exit("CENSUS_API_KEY not set (environment or .Renviron)")
    return key


# state FIPS -> postal (matches STATES in fetch_data.R)
FIPS = {
    "01": "AL", "02": "AK", "04": "AZ", "05": "AR", "06": "CA", "08": "CO",
    "09": "CT", "10": "DE", "11": "DC", "12": "FL", "13": "GA", "15": "HI",
    "16": "ID", "17": "IL", "18": "IN", "19": "IA", "20": "KS", "21": "KY",
    "22": "LA", "23": "ME", "24": "MD", "25": "MA", "26": "MI", "27": "MN",
    "28": "MS", "29": "MO", "30": "MT", "31": "NE", "32": "NV", "33": "NH",
    "34": "NJ", "35": "NM", "36": "NY", "37": "NC", "38": "ND", "39": "OH",
    "40": "OK", "41": "OR", "42": "PA", "44": "RI", "45": "SC", "46": "SD",
    "47": "TN", "48": "TX", "49": "UT", "50": "VT", "51": "VA", "53": "WA",
    "54": "WV", "55": "WI", "56": "WY",
}

B25070_VARS = ["B25070_001E", "B25070_007E", "B25070_008E",
               "B25070_009E", "B25070_010E", "B25070_011E"]
B19080_VARS = ["B19080_001E", "B19080_004E"]


def fetch(url, tries=3):
    for i in range(tries):
        try:
            with urllib.request.urlopen(url, timeout=60) as r:
                return json.load(r)
        except Exception as e:  # noqa: BLE001 — retry any transient failure
            if i == tries - 1:
                raise
            print(f"    retry after: {e}")
            time.sleep(2 ** (i + 1))


def year_rows(year, key):
    """-> {series_id: [(code, value), ...]} for one ACS 1-year vintage."""
    base = f"https://api.census.gov/data/{year}/acs/acs1"
    varlist = ",".join(B19080_VARS + B25070_VARS)
    out = {"income_20th": [], "income_80th": [], "rent_burden": []}
    for geo in ("us:1", "state:*"):
        url = f"{base}?get={varlist}&for={geo}&key={key}"
        data = fetch(url)
        hdr = data[0]
        for row in data[1:]:
            rec = dict(zip(hdr, row))
            if geo == "us:1":
                code = "US"
            else:
                code = FIPS.get(rec.get("state", ""))
                if not code:
                    continue  # Puerto Rico etc.

            def num(v):
                try:
                    x = float(rec[v])
                except (TypeError, ValueError):
                    return None
                # ACS annotation values (-666666666 etc.) mean suppressed
                return x if x > -100000 else None

            p20, p80 = num("B19080_001E"), num("B19080_004E")
            if p20 is not None:
                out["income_20th"].append((code, int(round(p20))))
            if p80 is not None:
                out["income_80th"].append((code, int(round(p80))))

            b = {v: num(v) for v in B25070_VARS}
            if all(b[v] is not None for v in B25070_VARS):
                denom = b["B25070_001E"] - b["B25070_011E"]
                if denom > 0:
                    burdened = (b["B25070_007E"] + b["B25070_008E"] +
                                b["B25070_009E"] + b["B25070_010E"])
                    out["rent_burden"].append((code, round(burdened / denom * 100, 1)))
    return out


def main():
    key = get_key()
    last_year = time.localtime().tm_year - 1
    os.makedirs(os.path.join("data", "census"), exist_ok=True)
    series = {"income_20th": [], "income_80th": [], "rent_burden": []}

    for year in range(FIRST_YEAR, last_year + 1):
        if year in SKIP_YEARS:
            continue
        try:
            got = year_rows(year, key)
        except Exception as e:  # noqa: BLE001
            # The newest vintage may simply not be out yet; anything else
            # about a past year should fail loudly.
            if year >= last_year:
                print(f"  {year}: not available yet ({e}) — stopping here")
                break
            raise
        for sid, rows in got.items():
            for code, val in rows:
                series[sid].append((f"{year}-01-01", code, val))
        print(f"  {year}: ok ({len(got['income_20th'])} geographies)")
        time.sleep(0.5)

    for sid, rows in series.items():
        if not rows:
            sys.exit(f"No data collected for {sid}")
        path = os.path.join("data", "census", f"{sid}.csv")
        with open(path, "w", newline="") as f:
            w = csv.writer(f)
            w.writerow(["date", "code", "value"])
            for date, code, val in sorted(rows, key=lambda r: (r[1], r[0])):
                w.writerow([date, code, val])
        n_codes = len({c for _, c, _ in rows})
        n_years = len({d for d, _, _ in rows})
        print(f"  census/{sid}.csv: {n_codes} geographies x {n_years} years")
    print("Done. Re-run `Rscript fetch_data.R` to embed into payloads.")


if __name__ == "__main__":
    main()
