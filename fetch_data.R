# fetch_data.R
# Downloads affordability data from FRED, BLS, Zillow, Census, and (optionally)
# EIA, and writes the static payloads the tracker reads.
#
# Usage:  Rscript fetch_data.R
#
# Outputs:
#   data/app_data.js            — national series (window.AFFORDABILITY_DATA)
#   data/states_index.js        — state/metric catalog (window.AFFORDABILITY_STATES)
#   data/states/{st}.js         — one state, all metrics (window.STATE_DATA[st])
#   data/state_metrics/{id}.js  — one metric, all states (window.STATE_METRIC[id])
#   data/{id}.csv               — per national series (download buttons / inspection)
#   data/state_{id}.csv         — wide date × state CSV per state metric
#   data/manifest.json          — metadata only, no data arrays
#
# Keys (read from .Renviron / environment):
#   BLS_KEY         — required: SA CPI subindexes not on FRED
#   CENSUS_API_KEY  — required: county renter-household weights for state rent
#   EIA_KEY         — optional: state average residential electricity bills;
#                     skipped loudly if absent

# Some shells (this machine's default, some CI runners) start R in the "C"
# locale, under which non-ASCII characters (en dashes, ÷) get written out as
# literal "<c3><b7>"-style escapes instead of real UTF-8 bytes. Force a UTF-8
# locale so labels/units render correctly regardless of the invoking shell.
for (loc in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
}

# NOTE: do NOT set a custom options(HTTPUserAgent) here. FRED's CDN rejects
# non-default user agents with an HTTP/2 framing error, while Zillow's static
# CSV host accepts R's default UA fine (verified 2026-07: only zillow.com's
# marketing pages bot-block, not files.zillowstatic.com).

# ── Package bootstrap ──────────────────────────────────────────────────────────
required <- c("jsonlite")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(pkg, repos = "https://cran.r-project.org/")
  }
}
suppressMessages(library(jsonlite))

`%||%` <- function(a, b) if (is.null(a)) b else a

# Drop NULL entries from a list before toJSON — jsonlite renders bare NULL as
# {}, which reads as truthy in JS and breaks `item.fred_id ?`-style checks.
compact <- function(x) Filter(Negate(is.null), x)

# ── Fetch helpers ──────────────────────────────────────────────────────────────

# Retry wrapper: transient network failures are the common case at ~250
# requests per refresh; 3 tries with exponential backoff before giving up.
with_retry <- function(expr_fn, what, tries = 3) {
  for (i in seq_len(tries)) {
    out <- tryCatch(expr_fn(), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    if (i < tries) Sys.sleep(2^i)
  }
  stop(what, ": ", conditionMessage(out))
}

# Fetch a FRED series as a data.frame using the public CSV endpoint (no API key)
fetch_fred <- function(fred_id, from = "2000-01-01") {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", fred_id)
  df <- with_retry(
    function() read.csv(url(url), stringsAsFactors = FALSE),
    paste0("FRED fetch failed for ", fred_id)
  )
  colnames(df) <- c("date", "value")
  df$value     <- suppressWarnings(as.numeric(df$value))
  df           <- df[!is.na(df$value) & df$date >= from, ]
  df
}

# Fetch a BLS series as a data.frame using the BLS Public Data API v2.
# Requires BLS_KEY in .Renviron. Used for series that aren't on FRED — in
# particular, seasonally-adjusted CPI subindexes (CUSR…) that FRED publishes
# only as not-seasonally-adjusted (CUUR…). The API limits each request to a
# 20-year window; we chunk if `from` is older than that.
fetch_bls <- function(bls_id, from = "2000-01-01") {
  api_key <- Sys.getenv("BLS_KEY")
  if (!nzchar(api_key)) stop("BLS_KEY not set in .Renviron")

  start_year <- as.integer(format(as.Date(from), "%Y"))
  end_year   <- as.integer(format(Sys.Date(),    "%Y"))

  all_obs <- list()
  win_start <- start_year
  while (win_start <= end_year) {
    win_end <- min(win_start + 19, end_year)

    payload <- toJSON(list(
      seriesid        = I(c(bls_id)),
      startyear       = as.character(win_start),
      endyear         = as.character(win_end),
      registrationkey = api_key
    ), auto_unbox = TRUE)

    tmp <- tempfile(fileext = ".json")
    writeLines(payload, tmp)

    # Note: R's system2 concatenates args with spaces and runs them through
    # the shell, so any arg that contains a space or special character must
    # be shQuote'd here or the shell will re-split it. Without quoting,
    # "Content-Type: application/json" becomes two tokens and curl drops
    # the header entirely (BLS then returns HTTP 415).
    raw <- system2(
      "curl",
      args = c("-s", "--max-time", "30", "-X", "POST",
               "-H", shQuote("Content-Type: application/json"),
               "--data-binary", shQuote(paste0("@", tmp)),
               shQuote("https://api.bls.gov/publicAPI/v2/timeseries/data/")),
      stdout = TRUE
    )
    unlink(tmp)
    if (length(raw) == 0) stop("BLS API: empty response (network or curl issue)")

    body <- paste(raw, collapse = "")
    parsed <- tryCatch(
      fromJSON(body, simplifyVector = FALSE),
      error = function(e) stop("BLS API: non-JSON response (first 200 chars): ",
                               substr(body, 1, 200))
    )
    if (!identical(parsed$status, "REQUEST_SUCCEEDED")) {
      stop("BLS API: ", paste(unlist(parsed$message), collapse = "; "))
    }

    series_list <- parsed$Results$series
    if (length(series_list) > 0 && length(series_list[[1]]$data) > 0) {
      all_obs <- c(all_obs, series_list[[1]]$data)
    }
    win_start <- win_end + 1
  }

  if (length(all_obs) == 0) stop("BLS API returned no observations for ", bls_id)

  rows <- lapply(all_obs, function(x) {
    p <- x$period
    date <- if (startsWith(p, "M") && p != "M13") {
      sprintf("%s-%02d-01", x$year, as.integer(substr(p, 2, 3)))
    } else if (startsWith(p, "Q")) {
      q <- as.integer(substr(p, 2, 3))
      sprintf("%s-%02d-01", x$year, (q - 1) * 3 + 1)
    } else if (startsWith(p, "S")) {
      sprintf("%s-%02d-01", x$year, if (p == "S01") 1 else 7)
    } else NULL
    if (is.null(date)) return(NULL)
    list(date = date, value = suppressWarnings(as.numeric(x$value)))
  })
  rows <- Filter(function(r) !is.null(r) && !is.na(r$value), rows)

  df <- data.frame(
    date  = vapply(rows, function(r) r$date,  character(1)),
    value = vapply(rows, function(r) r$value, numeric(1)),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$date), ]
  df <- df[df$date >= from, ]
  df
}

# ── National series configuration ─────────────────────────────────────────────
# To add a new item: append an entry here. The HTML picks it up automatically
# via manifest.json / app_data.js — no HTML edits required.
#
# Fields:
#   id           : slug used for filenames and JS lookups
#   fred_id      : FRED series identifier
#   label        : short display name
#   subtitle     : one-line description shown under the chart title
#   category     : chip grouping ("daily" | "groceries" | "big" | "labor" | "debt")
#   units        : axis label / tooltip suffix
#   description  : longer text shown in tooltip / card header
#   color        : hex color for the chart line. Colors are assigned in
#                  families by category (daily = teal, groceries = amber,
#                  big-ticket = indigo/violet, labor = green, debt = red) so
#                  related series read together — a comms-team request.
#   from         : earliest data date to fetch
#   is_new       : optional — TRUE flags a series for the "Newest" filter
#   invert_color : optional — TRUE means a rising value is good news (renders
#                  green, not red); used for wage/labor-demand series
#   scale        : optional — multiplier applied to fetched values (e.g. 0.001
#                  to show millions-of-dollars series in billions)
#   overlay_only : optional — TRUE means the series exists as a national
#                  comparison line for state charts and is not rendered as
#                  its own card

SERIES <- list(
  # ── Daily Items (teal family) ──
  list(
    id          = "groceries",
    fred_id     = "CPIFABSL",
    label       = "Groceries",
    subtitle    = "Food at Home CPI",
    category    = "daily",
    units       = "Index (1982–84 = 100)",
    description = "CPI for all urban consumers: food at home. Tracks how much grocery prices have risen relative to a 1982–84 baseline.",
    color       = "#0F766E",
    from        = "2000-01-01"
  ),
  list(
    id          = "gas",
    fred_id     = "GASREGCOVW",
    label       = "Gasoline",
    subtitle    = "Regular Unleaded, US Average",
    category    = "daily",
    units       = "$ per Gallon",
    description = "Weekly retail price of regular unleaded gasoline, averaged across all US regions. Published by the EIA.",
    color       = "#0D9488",
    from        = "2000-01-01"
  ),
  list(
    id          = "electricity",
    fred_id     = "CUSR0000SEHF01",
    label       = "Electricity",
    subtitle    = "Electricity CPI",
    category    = "daily",
    units       = "Index (1982–84 = 100)",
    description = "CPI for electricity, all urban consumers, seasonally adjusted.",
    color       = "#14B8A6",
    from        = "2000-01-01"
  ),
  # ── Groceries (amber/orange family) ──
  list(
    id          = "eggs",
    fred_id     = "APU0000708111",
    label       = "Eggs",
    subtitle    = "Grade A, Large, per Dozen",
    category    = "groceries",
    units       = "$ per Dozen",
    description = "Average retail price of Grade A large eggs in U.S. city averages, from the BLS Average Price data.",
    color       = "#F59E0B",
    from        = "2000-01-01"
  ),
  list(
    id          = "ground_beef",
    fred_id     = "APU0000703112",
    label       = "Ground Beef",
    subtitle    = "100% Beef, per Pound",
    category    = "groceries",
    units       = "$ per Pound",
    description = "Average retail price of 100% ground beef in U.S. city averages, from the BLS Average Price data.",
    color       = "#92400E",
    from        = "2000-01-01"
  ),
  list(
    id          = "chicken_breast",
    source      = "bls",
    bls_id      = "APU0000FF1101",
    fred_id     = "APU0000FF1101",
    label       = "Chicken Breast",
    subtitle    = "Boneless, per Pound",
    category    = "groceries",
    units       = "$ per Pound",
    description = "Average retail price of boneless chicken breast in U.S. city averages, from the BLS Average Price data.",
    color       = "#F97316",
    from        = "2006-01-01"
  ),
  list(
    id          = "milk",
    source      = "bls",
    bls_id      = "APU0000709112",
    fred_id     = "APU0000709112",
    label       = "Milk",
    subtitle    = "Whole, Fortified, per Gallon",
    category    = "groceries",
    units       = "$ per Gallon",
    description = "Average retail price of fresh whole fortified milk in U.S. city averages, from the BLS Average Price data.",
    color       = "#FBBF24",
    from        = "2000-01-01"
  ),
  list(
    id          = "bread",
    source      = "bls",
    bls_id      = "APU0000702111",
    fred_id     = "APU0000702111",
    label       = "Bread",
    subtitle    = "White, Pan, per Pound",
    category    = "groceries",
    units       = "$ per Pound",
    description = "Average retail price of white pan bread in U.S. city averages, from the BLS Average Price data.",
    color       = "#D97706",
    from        = "2000-01-01"
  ),
  list(
    id          = "bananas",
    source      = "bls",
    bls_id      = "APU0000711211",
    fred_id     = "APU0000711211",
    label       = "Bananas",
    subtitle    = "per Pound",
    category    = "groceries",
    units       = "$ per Pound",
    description = "Average retail price of bananas in U.S. city averages, from the BLS Average Price data.",
    color       = "#EAB308",
    from        = "2000-01-01"
  ),
  list(
    id          = "potatoes",
    source      = "bls",
    bls_id      = "APU0000712112",
    fred_id     = "APU0000712112",
    label       = "Potatoes",
    subtitle    = "White, per Pound",
    category    = "groceries",
    units       = "$ per Pound",
    description = "Average retail price of white potatoes in U.S. city averages, from the BLS Average Price data.",
    color       = "#B45309",
    from        = "2000-01-01"
  ),
  # ── Big Items (indigo/violet family) ──
  list(
    id          = "car_insurance",
    source      = "bls",
    bls_id      = "CUSR0000SETE",
    fred_id     = "CUUR0000SETE",   # NSA version on FRED, used for the "view source" link
    label       = "Car Insurance",
    subtitle    = "Motor Vehicle Insurance CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for motor vehicle insurance, all urban consumers, seasonally adjusted (fetched via the BLS API; FRED carries only the NSA version).",
    color       = "#6366F1",
    from        = "2000-01-01"
  ),
  list(
    id          = "new_cars",
    fred_id     = "CUSR0000SETA01",
    label       = "New Cars",
    subtitle    = "New Vehicles CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for new vehicles, all urban consumers, seasonally adjusted.",
    color       = "#7C3AED",
    from        = "2000-01-01"
  ),
  list(
    id          = "used_cars",
    fred_id     = "CUSR0000SETA02",
    label       = "Used Cars",
    subtitle    = "Used Cars and Trucks CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for used cars and trucks, all urban consumers, seasonally adjusted.",
    color       = "#A855F7",
    from        = "2000-01-01"
  ),
  list(
    id          = "health_insurance",
    source      = "bls",
    bls_id      = "CUSR0000SEMF",
    fred_id     = "CUUR0000SEMF",
    label       = "Health Insurance",
    subtitle    = "Health Insurance CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for health insurance, all urban consumers, seasonally adjusted (fetched via the BLS API). Captures the cost of insurance retained by insurers (administrative costs, profit), not total medical care.",
    color       = "#4F46E5",
    from        = "2005-12-01"
  ),
  list(
    id          = "childcare",
    source      = "bls",
    bls_id      = "CUSR0000SEEB03",
    fred_id     = "CUUR0000SEEB03",
    label       = "Childcare",
    subtitle    = "Day Care and Preschool CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for day care and preschool services, all urban consumers, seasonally adjusted (fetched via the BLS API).",
    color       = "#C026D3",
    from        = "2000-01-01"
  ),
  list(
    id          = "rent",
    fred_id     = "CUUR0000SEHA",
    label       = "Rent (CPI)",
    subtitle    = "Rent of Primary Residence CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for rent of primary residence, all urban consumers, not seasonally adjusted. Tracks rents paid by all tenants, including long-standing leases.",
    color       = "#4338CA",
    from        = "2000-01-01"
  ),
  list(
    id          = "zori_rent",
    source      = "zori",
    label       = "Rent (Market)",
    subtitle    = "Zillow Observed Rent Index, US",
    category    = "big",
    units       = "$ per Month",
    description = "Typical market-rate asking rent (smoothed, seasonally adjusted), all homes and apartments. Zillow Observed Rent Index. Unlike CPI rent, this tracks what a new lease costs today, in dollars.",
    color       = "#7E22CE",
    from        = "2015-01-01",
    is_new      = TRUE
  ),
  list(
    id          = "median_home_price",
    fred_id     = "MSPUS",
    label       = "Median Home Price",
    subtitle    = "Median Sales Price, Houses Sold (US)",
    category    = "big",
    units       = "$",
    description = "Median sales price of houses sold in the United States. Quarterly, from the Census Bureau / HUD.",
    color       = "#312E81",
    from        = "2000-01-01"
  ),
  list(
    id          = "mortgage",
    fred_id     = "MORTGAGE30US",
    label       = "30-Year Mortgage",
    subtitle    = "Freddie Mac Weekly Survey",
    category    = "big",
    units       = "Rate (%)",
    description = "Average 30-year fixed-rate mortgage as reported in the Freddie Mac Primary Mortgage Market Survey.",
    color       = "#5B21B6",
    from        = "2000-01-01"
  ),
  # ── Health coverage (KFF, violet family) ──
  # Annual, from data/kff/*.csv (scripts/fetch_kff.py). Dollar/percent levels,
  # not index series, so they display like the other $ / rate cards.
  list(
    id          = "aca_benchmark_premium",
    source      = "kff",
    kff_id      = "aca_benchmark_premium",
    label       = "ACA Benchmark Premium",
    subtitle    = "Second-Lowest-Cost Silver, Age 40",
    category    = "big",
    units       = "$ per Month",
    description = "Average benchmark premium — the second-lowest-cost silver Marketplace plan for a 40-year-old, the plan used to set ACA premium subsidies. KFF analysis of Healthcare.gov and state rate filings.",
    color       = "#9333EA",
    from        = "2018-01-01",
    is_new      = TRUE,
    source_note = "KFF State Health Facts, Average Marketplace Premiums by Metal Tier (annual)"
  ),
  list(
    id          = "uninsured_rate",
    source      = "kff",
    kff_id      = "uninsured_rate",
    label       = "Uninsured Rate",
    subtitle    = "Share of Population Without Coverage",
    category    = "big",
    units       = "Rate (%)",
    description = "Share of the total population with no health insurance coverage. KFF State Health Facts, Health Insurance Coverage of the Total Population (KFF estimates from the Census ACS). No data for 2020, which the ACS did not release on a comparable basis.",
    color       = "#A21CAF",
    from        = "2008-01-01",
    is_new      = TRUE,
    source_note = "KFF State Health Facts, Health Insurance Coverage of the Total Population (annual)"
  ),
  # ── Labor Market (green family) ──
  list(
    id          = "unemployment",
    fred_id     = "UNRATE",
    label       = "Unemployment Rate",
    subtitle    = "US Civilian Unemployment",
    category    = "labor",
    units       = "Rate (%)",
    description = "Percent of the labor force that is unemployed and actively seeking work. Monthly, seasonally adjusted.",
    color       = "#065F46",
    from        = "2000-01-01"
  ),
  list(
    id          = "hourly_earnings",
    fred_id     = "CES0500000003",
    label       = "Hourly Earnings",
    subtitle    = "Average, All Private Employees",
    category    = "labor",
    units       = "$ per Hour",
    description = "Average hourly earnings of all private-sector employees. A key measure of wage growth and purchasing power.",
    color       = "#10B981",
    from        = "2000-01-01",
    invert_color = TRUE
  ),
  list(
    id          = "job_openings",
    fred_id     = "JTSJOL",
    label       = "Job Openings",
    subtitle    = "JOLTS Survey, Total Nonfarm",
    category    = "labor",
    units       = "Thousands of Jobs",
    description = "Total nonfarm job openings from the Job Openings and Labor Turnover Survey (JOLTS). A signal of labor demand.",
    color       = "#34D399",
    from        = "2000-01-01",
    invert_color = TRUE
  ),
  list(
    id          = "quits_rate",
    fred_id     = "JTSQUR",
    label       = "Quit Rate",
    subtitle    = "JOLTS Survey, Total Nonfarm",
    category    = "labor",
    units       = "Rate (%)",
    description = "Quits as a percent of total employment, total nonfarm, monthly, seasonally adjusted (JOLTS). A higher quit rate indicates worker confidence in finding new jobs.",
    color       = "#047857",
    from        = "2000-01-01",
    invert_color = TRUE
  ),
  list(
    id          = "median_weeks_unemployed",
    fred_id     = "UEMPMED",
    label       = "Median Weeks Unemployed",
    subtitle    = "Median Duration of Unemployment",
    category    = "labor",
    units       = "Weeks",
    description = "Median number of weeks an unemployed person has been seeking work, monthly, seasonally adjusted.",
    color       = "#064E3B",
    from        = "2000-01-01"
  ),
  list(
    id          = "mean_weeks_unemployed",
    fred_id     = "UEMPMEAN",
    label       = "Mean Weeks Unemployed",
    subtitle    = "Average Duration of Unemployment",
    category    = "labor",
    units       = "Weeks",
    description = "Average number of weeks an unemployed person has been seeking work, monthly, seasonally adjusted.",
    color       = "#22C55E",
    from        = "2000-01-01"
  ),
  list(
    id          = "cpi_all_items",
    fred_id     = "CPIAUCSL",
    label       = "Inflation (CPI)",
    subtitle    = "All Items, Seasonally Adjusted",
    category    = "labor",
    units       = "Index (1982–84 = 100)",
    description = "CPI for all urban consumers, all items, seasonally adjusted — the broadest measure of the price level. Used here to compute real (inflation-adjusted) wages.",
    color       = "#64748B",
    from        = "2000-01-01"
  ),
  # ── Debt (red family) ──
  # NY Fed Household Debt & Credit tracks these same balances at higher
  # resolution but only publishes Excel files; these FRED series (Fed G.19
  # release) cover the same concepts and let the existing CSV fetcher work
  # unchanged. SLOASM/MVLOASM are the monthly successors to the discontinued
  # quarterly SLOAS/MVLOAS series (verified current as of this fetch).
  list(
    id          = "revolving_credit",
    fred_id     = "REVOLSL",
    label       = "Revolving Credit",
    subtitle    = "Credit Cards & Other Revolving Debt",
    category    = "debt",
    units       = "$ Billions",
    description = "Total revolving consumer credit outstanding (mostly credit cards), owned and securitized. Federal Reserve G.19 release.",
    color       = "#DC2626",
    from        = "2000-01-01",
    scale       = 0.001
  ),
  list(
    id          = "student_loans",
    fred_id     = "SLOASM",
    label       = "Student Loans",
    subtitle    = "Student Loan Debt Outstanding",
    category    = "debt",
    units       = "$ Billions",
    description = "Total student loan debt owned and securitized. Federal Reserve G.19 release.",
    color       = "#991B1B",
    from        = "2006-01-01",
    scale       = 0.001
  ),
  list(
    id          = "auto_loans",
    fred_id     = "MVLOASM",
    label       = "Auto Loans",
    subtitle    = "Motor Vehicle Loan Debt Outstanding",
    category    = "debt",
    units       = "$ Billions",
    description = "Total motor vehicle loan debt owned and securitized. Federal Reserve G.19 release.",
    color       = "#EF4444",
    from        = "2000-01-01",
    scale       = 0.001
  ),
  list(
    id          = "credit_card_delinquency",
    fred_id     = "DRCCLACBS",
    label       = "Credit Card Delinquency",
    subtitle    = "Delinquency Rate, All Commercial Banks",
    category    = "debt",
    units       = "Rate (%)",
    description = "Share of credit card loan balances 30+ days delinquent at all commercial banks. Quarterly, seasonally adjusted.",
    color       = "#B91C1C",
    from        = "2000-01-01"
  ),
  list(
    id          = "consumer_loan_delinquency",
    fred_id     = "DRCLACBS",
    label       = "Consumer Loan Delinquency",
    subtitle    = "Delinquency Rate, All Commercial Banks",
    category    = "debt",
    units       = "Rate (%)",
    description = "Share of consumer loan balances 30+ days delinquent at all commercial banks. Quarterly, seasonally adjusted.",
    color       = "#F87171",
    from        = "2000-01-01"
  ),
  list(
    id          = "mortgage_delinquency",
    fred_id     = "DRSFRMACBS",
    label       = "Mortgage Delinquency",
    subtitle    = "Single-Family Residential, All Commercial Banks",
    category    = "debt",
    units       = "Rate (%)",
    description = "Share of single-family residential mortgage balances 30+ days delinquent at all commercial banks. Quarterly, seasonally adjusted.",
    color       = "#7F1D1D",
    from        = "2000-01-01"
  ),
  # ── National overlays for state charts (not rendered as cards) ──
  list(
    id           = "us_home_price_index",
    fred_id      = "USSTHPI",
    label        = "US Home Prices",
    subtitle     = "FHFA All-Transactions HPI, US",
    category     = "big",
    units        = "Index (1980 Q1 = 100)",
    description  = "FHFA All-Transactions House Price Index for the United States, quarterly, not seasonally adjusted. Used as the national comparison line on state home-price charts.",
    color        = "#312E81",
    from         = "2000-01-01",
    overlay_only = TRUE
  ),
  list(
    id           = "us_median_income",
    fred_id      = "MEHOINUSA672N",
    label        = "US Median Income",
    subtitle     = "Real Median Household Income, US",
    category     = "labor",
    units        = "$",
    description  = "Real (CPI-U-RS-adjusted) median household income in the United States, annual, from the Census Bureau. Used as the national comparison line on state income charts.",
    color        = "#7C3AED",
    from         = "2000-01-01",
    invert_color = TRUE,
    overlay_only = TRUE
  )
  # ── Add future national series above this comment ──
)

# ── State configuration ────────────────────────────────────────────────────────
# All 50 states + DC. FRED ID patterns (each verified against fredgraph.csv
# for edge cases — DC, VT — before adoption; the fetch loop fails loudly on
# any ID that doesn't resolve):
#   unemployment : {ST}UR              — LAUS unemployment rate, monthly SA
#   wages        : SMU{fips}000000500000003 — CES avg hourly earnings, total
#                  private, monthly NSA (starts 2007)
#   home_prices  : {ST}STHPI           — FHFA All-Transactions HPI, quarterly
#   income       : MEHOINUS{ST}A672N   — real median household income, annual
# Rent comes from Zillow ZORI county data aggregated with ACS renter-household
# weights (see below). No state CPI exists — the tracker must never imply one.

STATES <- list(
  list(code="AL", name="Alabama",        fips="01"), list(code="AK", name="Alaska",         fips="02"),
  list(code="AZ", name="Arizona",        fips="04"), list(code="AR", name="Arkansas",       fips="05"),
  list(code="CA", name="California",     fips="06"), list(code="CO", name="Colorado",       fips="08"),
  list(code="CT", name="Connecticut",    fips="09"), list(code="DE", name="Delaware",       fips="10"),
  list(code="DC", name="District of Columbia", fips="11"),
  list(code="FL", name="Florida",        fips="12"), list(code="GA", name="Georgia",        fips="13"),
  list(code="HI", name="Hawaii",         fips="15"), list(code="ID", name="Idaho",          fips="16"),
  list(code="IL", name="Illinois",       fips="17"), list(code="IN", name="Indiana",        fips="18"),
  list(code="IA", name="Iowa",           fips="19"), list(code="KS", name="Kansas",         fips="20"),
  list(code="KY", name="Kentucky",       fips="21"), list(code="LA", name="Louisiana",      fips="22"),
  list(code="ME", name="Maine",          fips="23"), list(code="MD", name="Maryland",       fips="24"),
  list(code="MA", name="Massachusetts",  fips="25"), list(code="MI", name="Michigan",       fips="26"),
  list(code="MN", name="Minnesota",      fips="27"), list(code="MS", name="Mississippi",    fips="28"),
  list(code="MO", name="Missouri",       fips="29"), list(code="MT", name="Montana",        fips="30"),
  list(code="NE", name="Nebraska",       fips="31"), list(code="NV", name="Nevada",         fips="32"),
  list(code="NH", name="New Hampshire",  fips="33"), list(code="NJ", name="New Jersey",     fips="34"),
  list(code="NM", name="New Mexico",     fips="35"), list(code="NY", name="New York",       fips="36"),
  list(code="NC", name="North Carolina", fips="37"), list(code="ND", name="North Dakota",   fips="38"),
  list(code="OH", name="Ohio",           fips="39"), list(code="OK", name="Oklahoma",       fips="40"),
  list(code="OR", name="Oregon",         fips="41"), list(code="PA", name="Pennsylvania",   fips="42"),
  list(code="RI", name="Rhode Island",   fips="44"), list(code="SC", name="South Carolina", fips="45"),
  list(code="SD", name="South Dakota",   fips="46"), list(code="TN", name="Tennessee",      fips="47"),
  list(code="TX", name="Texas",          fips="48"), list(code="UT", name="Utah",           fips="49"),
  list(code="VT", name="Vermont",        fips="50"), list(code="VA", name="Virginia",       fips="51"),
  list(code="WA", name="Washington",     fips="53"), list(code="WV", name="West Virginia",  fips="54"),
  list(code="WI", name="Wisconsin",      fips="55"), list(code="WY", name="Wyoming",        fips="56")
)

# State metric catalog. `national_id` names the national series (in SERIES)
# drawn as the dashed US comparison line on state charts.
STATE_METRICS <- list(
  list(
    id           = "unemployment",
    label        = "Unemployment Rate",
    units        = "Rate (%)",
    color        = "#065F46",
    national_id  = "unemployment",
    frequency    = "Monthly",
    source_label = "BLS LAUS via FRED",
    description  = "Unemployment rate, seasonally adjusted. BLS Local Area Unemployment Statistics.",
    fred_pattern = function(s) paste0(s$code, "UR"),
    from         = "2000-01-01",
    round_digits = 1
  ),
  list(
    id           = "wages",
    label        = "Hourly Wages",
    units        = "$ per Hour",
    color        = "#10B981",
    national_id  = "hourly_earnings",
    frequency    = "Monthly",
    source_label = "BLS State CES via FRED",
    description  = "Average hourly earnings of all private employees. BLS state Current Employment Statistics, not seasonally adjusted.",
    fred_pattern = function(s) paste0("SMU", s$fips, "000000500000003"),
    from         = "2007-01-01",
    invert_color = TRUE,
    round_digits = 2
  ),
  list(
    id           = "home_prices",
    label        = "Home Prices",
    units        = "Index (1980 Q1 = 100)",
    color        = "#4338CA",
    national_id  = "us_home_price_index",
    frequency    = "Quarterly",
    source_label = "FHFA via FRED",
    description  = "FHFA All-Transactions House Price Index, quarterly, not seasonally adjusted.",
    fred_pattern = function(s) paste0(s$code, "STHPI"),
    from         = "2000-01-01",
    round_digits = 2
  ),
  list(
    id           = "income",
    label        = "Median Household Income",
    units        = "$",
    color        = "#7C3AED",
    national_id  = "us_median_income",
    frequency    = "Annual",
    source_label = "Census via FRED",
    description  = "Real (CPI-U-RS-adjusted) median household income, annual, from the Census Bureau Current Population Survey.",
    fred_pattern = function(s) paste0("MEHOINUS", s$code, "A672N"),
    from         = "2000-01-01",
    invert_color = TRUE,
    round_digits = 0
  ),
  list(
    id           = "rent",
    label        = "Rent (Market)",
    units        = "$ per Month",
    color        = "#7E22CE",
    national_id  = "zori_rent",
    frequency    = "Monthly",
    source_label = "Zillow ZORI (county data, renter-weighted)",
    description  = "Typical market-rate asking rent. Zillow does not publish state ZORI, so this aggregates Zillow's county-level index using ACS renter-household weights; states where covered counties hold under half of renter households are omitted.",
    source       = "zori",
    from         = "2015-01-01",
    round_digits = 0
  ),
  list(
    id           = "electricity_bill",
    label        = "Electricity Bill",
    units        = "$ per Month",
    color        = "#14B8A6",
    national_id  = NULL,
    frequency    = "Monthly",
    source_label = "EIA-861M",
    description  = "Average monthly residential electricity bill: total residential revenue divided by residential customer count, from EIA Form 861M. Recent months are preliminary and revised later.",
    source       = "eia",
    from         = "2008-01-01",
    round_digits = 2
  ),
  list(
    id           = "aca_benchmark_premium",
    label        = "ACA Benchmark Premium",
    units        = "$ per Month",
    color        = "#9333EA",
    national_id  = "aca_benchmark_premium",
    frequency    = "Annual",
    source_label = "KFF State Health Facts",
    description  = "Average benchmark premium — the second-lowest-cost silver Marketplace plan for a 40-year-old, the plan ACA subsidies are pegged to. KFF analysis of Healthcare.gov and state rate filings.",
    source       = "kff",
    kff_id       = "aca_benchmark_premium",
    from         = "2018-01-01",
    round_digits = 0
  ),
  list(
    id           = "uninsured_rate",
    label        = "Uninsured Rate",
    units        = "Rate (%)",
    color        = "#A21CAF",
    national_id  = "uninsured_rate",
    frequency    = "Annual",
    source_label = "KFF State Health Facts",
    description  = "Share of the total population with no health insurance coverage. KFF Health Insurance Coverage of the Total Population. No 2020 data point (ACS did not release comparably).",
    source       = "kff",
    kff_id       = "uninsured_rate",
    from         = "2008-01-01",
    round_digits = 1
  )
)

# ── ZORI: Zillow county rents aggregated to states ────────────────────────────
# Zillow publishes ZORI at county level (plus a US row in the metro file) but
# not by state. We aggregate counties to states with fixed ACS renter-household
# weights — the appropriate universe, since ZORI measures rental listings.
# Each state's coverage (share of renter households in ZORI-covered counties)
# is recorded; states under 50% coverage are dropped rather than shown thin.
ZORI_COUNTY_URL <- "https://files.zillowstatic.com/research/public_csvs/zori/County_zori_uc_sfrcondomfr_sm_sa_month.csv"
ZORI_METRO_URL  <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_sa_month.csv"
ZORI_MIN_COVERAGE <- 0.5

fetch_renter_weights <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key)) stop("CENSUS_API_KEY not set — required for ZORI state aggregation")
  # ACS 5-year renter-occupied housing units (B25003_003) per county. Fixed
  # weights from a single vintage: we want a stable aggregation, not one
  # whose composition shifts under the rent series each year.
  u <- paste0("https://api.census.gov/data/2023/acs/acs5?get=B25003_003E&for=county:*&key=", key)
  m <- with_retry(function() fromJSON(u), "Census ACS renter-household fetch")
  df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
  colnames(df) <- m[1, ]
  out <- data.frame(
    fips_full = paste0(df$state, df$county),
    renters   = suppressWarnings(as.numeric(df$B25003_003E)),
    stringsAsFactors = FALSE
  )

  # Connecticut: ACS 2023 reports CT as planning regions (FIPS 09110+), but
  # Zillow's county file still uses the legacy county codes (09001–09015).
  # Swap in CT weights from ACS 2021, the last vintage on legacy counties,
  # so the join works and CT's denominator isn't double-counted.
  u_ct <- paste0("https://api.census.gov/data/2021/acs/acs5?get=B25003_003E&for=county:*&in=state:09&key=", key)
  m_ct <- with_retry(function() fromJSON(u_ct), "Census ACS 2021 CT renter-household fetch")
  df_ct <- as.data.frame(m_ct[-1, , drop = FALSE], stringsAsFactors = FALSE)
  colnames(df_ct) <- m_ct[1, ]
  out <- rbind(
    out[substr(out$fips_full, 1, 2) != "09", ],
    data.frame(
      fips_full = paste0(df_ct$state, df_ct$county),
      renters   = suppressWarnings(as.numeric(df_ct$B25003_003E)),
      stringsAsFactors = FALSE
    )
  )
  out
}

fetch_zori <- function() {
  raw <- with_retry(
    function() read.csv(url(ZORI_COUNTY_URL), check.names = FALSE, stringsAsFactors = FALSE),
    "Zillow county ZORI fetch"
  )
  date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(raw), value = TRUE)
  if (length(date_cols) < 12) stop("ZORI county file: unexpected format (", length(date_cols), " date columns)")

  weights <- fetch_renter_weights()
  raw$fips_full <- paste0(
    sprintf("%02d", as.integer(raw$StateCodeFIPS)),
    sprintf("%03d", as.integer(raw$MunicipalCodeFIPS))
  )
  raw$renters <- weights$renters[match(raw$fips_full, weights$fips_full)]
  raw$renters[is.na(raw$renters)] <- 0

  fips_to_code <- setNames(
    vapply(STATES, function(s) s$code, character(1)),
    vapply(STATES, function(s) s$fips, character(1))
  )
  raw$state_code <- fips_to_code[sprintf("%02d", as.integer(raw$StateCodeFIPS))]

  # Total renter households per state (all counties, not just ZORI-covered)
  state_totals <- tapply(weights$renters,
                         substr(weights$fips_full, 1, 2), sum, na.rm = TRUE)

  out <- list()
  for (s in STATES) {
    rows <- raw[!is.na(raw$state_code) & raw$state_code == s$code, ]
    total_renters <- state_totals[[s$fips]]
    if (nrow(rows) == 0 || is.null(total_renters) || total_renters == 0) next

    # Coverage: renter households in counties with a current ZORI value
    last_col <- date_cols[length(date_cols)]
    covered  <- sum(rows$renters[!is.na(rows[[last_col]])], na.rm = TRUE)
    coverage <- covered / total_renters
    if (coverage < ZORI_MIN_COVERAGE) {
      cat(sprintf("  ZORI: skipping %s (coverage %.0f%% < %.0f%%)\n",
                  s$code, coverage * 100, ZORI_MIN_COVERAGE * 100))
      next
    }

    vals <- vapply(date_cols, function(col) {
      v <- rows[[col]]; w <- rows$renters
      ok <- !is.na(v) & w > 0
      if (!any(ok)) return(NA_real_)
      sum(v[ok] * w[ok]) / sum(w[ok])
    }, numeric(1))

    df <- data.frame(
      # Zillow stamps month-end dates; normalize to first-of-month like
      # every other monthly series here.
      date  = paste0(substr(date_cols, 1, 7), "-01"),
      value = round(vals, 0),
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$value), ]
    out[[s$code]] <- list(data = df, coverage = round(coverage, 3))
  }

  # US national row from the metro file
  metro <- with_retry(
    function() read.csv(url(ZORI_METRO_URL), check.names = FALSE, stringsAsFactors = FALSE),
    "Zillow metro ZORI fetch (US row)"
  )
  us_row <- metro[metro$RegionType == "country", , drop = FALSE]
  if (nrow(us_row) == 1) {
    mcols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(metro), value = TRUE)
    us_df <- data.frame(
      date  = paste0(substr(mcols, 1, 7), "-01"),
      value = round(as.numeric(us_row[1, mcols]), 0),
      stringsAsFactors = FALSE
    )
    out[["US"]] <- list(data = us_df[!is.na(us_df$value), ], coverage = 1)
  } else {
    stop("ZORI metro file: could not find United States row")
  }
  out
}

# ── EIA-861M: average residential electricity bills by state (optional) ───────
# Requires a free EIA API key (https://www.eia.gov/opendata/). Skipped, loudly
# but without failing the run, when EIA_KEY is absent — the tracker simply
# won't have the electricity_bill state metric until the key is added.
fetch_eia_bills <- function() {
  key <- Sys.getenv("EIA_KEY")
  if (!nzchar(key)) {
    cat("\nEIA_KEY not set — skipping state electricity bills.",
        "\nRegister a free key at https://www.eia.gov/opendata/ and add",
        "EIA_KEY to .Renviron (and the GitHub Actions secrets) to enable.\n")
    return(NULL)
  }

  base <- paste0(
    "https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=", key,
    "&frequency=monthly&data[0]=revenue&data[1]=customers",
    "&facets[sectorid][]=RES&start=2008-01&sort[0][column]=period&sort[0][direction]=asc"
  )
  rows <- list(); offset <- 0
  repeat {
    u <- paste0(base, "&offset=", offset, "&length=5000")
    resp <- with_retry(function() fromJSON(u), "EIA retail-sales fetch")
    chunk <- resp$response$data
    if (is.null(chunk) || nrow(chunk) == 0) break
    rows[[length(rows) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (nrow(chunk) < 5000) break
  }
  if (length(rows) == 0) stop("EIA API returned no rows")
  d <- do.call(rbind, rows)

  # revenue is in million $; customers is a count. Average monthly bill =
  # revenue * 1e6 / customers.
  d$revenue   <- suppressWarnings(as.numeric(d$revenue))
  d$customers <- suppressWarnings(as.numeric(d$customers))
  d <- d[!is.na(d$revenue) & !is.na(d$customers) & d$customers > 0, ]
  d$bill <- round(d$revenue * 1e6 / d$customers, 2)
  d$date <- paste0(d$period, "-01")

  codes <- c(vapply(STATES, function(s) s$code, character(1)), "US")
  out <- list()
  for (code in codes) {
    sub <- d[d$stateid == code, c("date", "bill")]
    if (nrow(sub) < 12) next
    sub <- sub[order(sub$date), ]
    colnames(sub) <- c("date", "value")
    out[[code]] <- list(data = sub, coverage = 1)
  }
  out
}

# ── Annual state layers (Phase 4) ─────────────────────────────────────────────
# Slow-moving, state-level context published yearly (childcare prices, ACA
# benchmark premiums, household debt). These are compiled by hand into
# data/annual/<id>.csv (columns: state,value) and described in
# data/annual/annual_meta.json:
#   [{ "id","label","units","year","source","source_url","file","higher_is" }]
# Anything present is attached to the state payloads; nothing here blocks the
# time-series refresh.
read_annual_layers <- function() {
  meta_path <- file.path("data", "annual", "annual_meta.json")
  if (!file.exists(meta_path)) return(list(meta = list(), values = list()))
  meta <- fromJSON(meta_path, simplifyVector = FALSE)
  values <- list()
  kept <- list()
  for (m in meta) {
    f <- file.path("data", "annual", m$file)
    if (!file.exists(f)) {
      cat(sprintf("  annual layer %s: %s missing, skipping\n", m$id, m$file))
      next
    }
    df <- read.csv(f, stringsAsFactors = FALSE)
    if (!all(c("state", "value") %in% names(df))) {
      cat(sprintf("  annual layer %s: needs state,value columns, skipping\n", m$id))
      next
    }
    values[[m$id]] <- setNames(as.list(df$value), df$state)
    kept[[length(kept) + 1]] <- m
    cat(sprintf("  annual layer %s: %d states (%s, %s)\n",
                m$id, nrow(df), m$year, m$source))
  }
  list(meta = kept, values = values)
}

# ── KFF annual time series ────────────────────────────────────────────────────
# Multi-year, state-level indicators pulled from KFF State Health Facts by the
# companion script scripts/fetch_kff.py (run yearly). Unlike the single-value
# annual layers above, these are full time series (one point per year) and feed
# both a national card and a state metric. Each file under data/kff/ is a long
# CSV with columns date,code,value (code = "US" or 2-letter postal). Returns a
# named list: kff[[id]][[code]] = data.frame(date, value) sorted by date.
# Committed artifacts — this reads them; it does not hit the network. If a file
# is absent the series simply won't build (and fails loudly downstream).
read_kff <- function() {
  dir <- file.path("data", "kff")
  if (!dir.exists(dir)) return(list())
  out <- list()
  for (f in list.files(dir, pattern = "\\.csv$", full.names = TRUE)) {
    id <- sub("\\.csv$", "", basename(f))
    df <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !all(c("date", "code", "value") %in% names(df))) {
      cat(sprintf("  KFF file %s: needs date,code,value columns, skipping\n", basename(f)))
      next
    }
    df <- df[!is.na(df$value), ]
    series <- list()
    for (code in unique(df$code)) {
      sub <- df[df$code == code, c("date", "value")]
      sub <- sub[order(sub$date), ]
      rownames(sub) <- NULL
      series[[code]] <- sub
    }
    out[[id]] <- series
  }
  out
}

# ── Helpers ───────────────────────────────────────────────────────────────────
yoy_pct <- function(df) {
  latest      <- tail(df, 1)
  cutoff      <- as.Date(latest$date) - 365
  prior_rows  <- df[as.Date(df$date) <= cutoff, ]
  if (nrow(prior_rows) == 0) return(NA_real_)
  prior_val   <- tail(prior_rows$value, 1)
  round((latest$value - prior_val) / prior_val * 100, 2)
}

failures <- character(0)

# ── National download loop ────────────────────────────────────────────────────
dir.create("data",               showWarnings = FALSE, recursive = TRUE)
dir.create("data/states",        showWarnings = FALSE, recursive = TRUE)
dir.create("data/state_metrics", showWarnings = FALSE, recursive = TRUE)

# ZORI is fetched once and reused for the national card and every state.
cat("Fetching Zillow ZORI (county + US) ...")
zori <- tryCatch(fetch_zori(), error = function(e) {
  cat(" ✗ ERROR:", conditionMessage(e), "\n")
  failures <<- c(failures, "zori")
  NULL
})
if (!is.null(zori)) cat(sprintf(" ✓  (%d states + US)\n", length(zori) - 1))

# KFF annual series (data/kff/*.csv), produced yearly by scripts/fetch_kff.py.
# Read once and reused for the national cards and every state metric below.
cat("Reading KFF annual series (data/kff/) ...")
kff <- read_kff()
cat(sprintf(" ✓  (%s)\n",
            if (length(kff)) paste(names(kff), collapse = ", ") else "none found"))

all_data <- list()

for (cfg in SERIES) {
  src_id <- if (!is.null(cfg$source) && cfg$source == "bls") cfg$bls_id else cfg$fred_id
  cat(sprintf("\nFetching %-22s (%s) ...", cfg$label, src_id %||% cfg$source))

  result <- tryCatch({
    df <- if (!is.null(cfg$source) && cfg$source == "bls") {
      fetch_bls(cfg$bls_id, from = cfg$from)
    } else if (!is.null(cfg$source) && cfg$source == "zori") {
      if (is.null(zori)) stop("ZORI fetch failed upstream")
      zori[["US"]]$data
    } else if (!is.null(cfg$source) && cfg$source == "kff") {
      ks <- kff[[cfg$kff_id]]
      if (is.null(ks) || is.null(ks[["US"]]))
        stop("KFF series missing — run scripts/fetch_kff.py: ", cfg$kff_id)
      d <- ks[["US"]]
      d[d$date >= cfg$from, ]
    } else {
      fetch_fred(cfg$fred_id, from = cfg$from)
    }

    # Optional unit-conversion multiplier (e.g. 0.001 to show a
    # millions-of-dollars series in billions). Purely a display scale —
    # doesn't touch the underlying FRED values' meaning.
    if (!is.null(cfg$scale)) df$value <- df$value * cfg$scale

    # Write per-series CSV — raw levels; the front end rebases index series
    # at view time based on the selected x-axis range.
    write.csv(df, file.path("data", paste0(cfg$id, ".csv")), row.names = FALSE)

    latest_val  <- tail(df$value, 1)
    latest_date <- tail(df$date,  1)
    change      <- yoy_pct(df)

    # `rebase = TRUE` tells the front end: don't display the raw index value;
    # show cumulative % change since the start of the visible x-axis window.
    rebase_flag <- grepl("^Index", cfg$units)

    entry <- c(
      compact(cfg[c("id", "label", "subtitle", "category", "units", "description", "color", "fred_id")]),
      compact(list(
        is_new       = isTRUE(cfg$is_new),
        invert_color = isTRUE(cfg$invert_color),
        overlay_only = isTRUE(cfg$overlay_only),
        rebase       = rebase_flag,
        source_note  = if (!is.null(cfg$source_note)) cfg$source_note
                       else if (!is.null(cfg$source) && cfg$source == "zori")
                         "Zillow Observed Rent Index (ZORI), smoothed & seasonally adjusted" else NULL,
        last_updated = format(Sys.Date(), "%Y-%m-%d"),
        latest_value = round(latest_val, 3),
        latest_date  = latest_date,
        yoy_change   = if (!is.na(change)) change else NULL,
        n_obs        = nrow(df),
        data         = df
      ))
    )
    cat(sprintf(" ✓  (%.3f on %s, YoY: %s%%)\n",
                latest_val, latest_date,
                if (!is.na(change)) sprintf("%+.1f", change) else "N/A"))
    entry
  }, error = function(e) {
    cat(sprintf(" ✗  ERROR: %s\n", conditionMessage(e)))
    NULL
  })

  if (!is.null(result)) all_data[[cfg$id]] <- result
}

# ── Derived series: real (inflation-adjusted) hourly earnings ──────────────────
# Nominal average hourly earnings deflated by CPI-U (all items), re-expressed
# in the latest month's dollars so the newest point matches the nominal
# series and earlier points show what that paycheck is worth today. This is
# the standard "real wage" construction; no separate wage price index exists.
if (!is.null(all_data$hourly_earnings) && !is.null(all_data$cpi_all_items)) {
  wage_df <- all_data$hourly_earnings$data
  cpi_df  <- all_data$cpi_all_items$data
  merged  <- merge(wage_df, cpi_df, by = "date", suffixes = c("_wage", "_cpi"))
  merged  <- merged[order(merged$date), ]
  latest_cpi <- tail(merged$value_cpi, 1)
  real_df <- data.frame(
    date  = merged$date,
    value = round(merged$value_wage * (latest_cpi / merged$value_cpi), 3)
  )

  write.csv(real_df, file.path("data", "real_hourly_earnings.csv"), row.names = FALSE)

  latest_val  <- tail(real_df$value, 1)
  latest_date <- tail(real_df$date,  1)
  change      <- yoy_pct(real_df)

  all_data$real_hourly_earnings <- list(
    id           = "real_hourly_earnings",
    label        = "Real Hourly Earnings",
    subtitle     = "Inflation-Adjusted, Today's Dollars",
    category     = "labor",
    units        = "$ per Hour",
    description  = "Average hourly earnings of all private-sector employees, deflated by CPI-U (all items, seasonally adjusted) and expressed in the most recent month's dollars. Shows whether paychecks are keeping up with prices.",
    color        = "#059669",
    source_note  = "Derived: BLS CES0500000003 ÷ FRED CPIAUCSL",
    is_new       = FALSE,
    invert_color = TRUE,
    overlay_only = FALSE,
    rebase       = FALSE,
    last_updated = format(Sys.Date(), "%Y-%m-%d"),
    latest_value = round(latest_val, 3),
    latest_date  = latest_date,
    yoy_change   = if (!is.na(change)) change else NULL,
    n_obs        = nrow(real_df),
    data         = real_df
  )
  cat(sprintf("\nDerived real_hourly_earnings ✓  (%.2f on %s)\n", latest_val, latest_date))
}

# ── State download loop ───────────────────────────────────────────────────────
# ~200 FRED requests; a short sleep keeps us polite to the public endpoint.
cat("\n── State series ─────────────────────────────\n")

eia <- NULL
state_results <- list()  # state_results[[code]][[metric_id]]

for (metric in STATE_METRICS) {
  cat(sprintf("\n%s (%s):\n", metric$label, metric$id))

  if (!is.null(metric$source) && metric$source == "zori") {
    if (is.null(zori)) { failures <- c(failures, "state_rent"); next }
    for (s in STATES) {
      z <- zori[[s$code]]
      if (is.null(z)) next
      state_results[[s$code]][[metric$id]] <- c(z, list(coverage = z$coverage))
    }
    cat(sprintf("  ✓ %d states from ZORI aggregation\n",
                sum(vapply(STATES, function(s) !is.null(zori[[s$code]]), logical(1)))))
    next
  }

  if (!is.null(metric$source) && metric$source == "eia") {
    if (is.null(eia)) {
      eia <- tryCatch(fetch_eia_bills(), error = function(e) {
        # Optional layer: a broken EIA feed shouldn't block the CPI refresh,
        # but say so loudly.
        cat("  ✗ EIA fetch failed:", conditionMessage(e), "— skipping electricity bills\n")
        NULL
      })
    }
    if (is.null(eia)) next
    for (s in STATES) {
      e <- eia[[s$code]]
      if (is.null(e)) next
      state_results[[s$code]][[metric$id]] <- e
    }
    cat(sprintf("  ✓ %d states from EIA-861M\n",
                sum(vapply(STATES, function(s) !is.null(eia[[s$code]]), logical(1)))))
    next
  }

  if (!is.null(metric$source) && metric$source == "kff") {
    ks <- kff[[metric$kff_id]]
    if (is.null(ks)) {
      cat("  ✗ KFF data missing — run scripts/fetch_kff.py; skipping\n")
      failures <- c(failures, paste0("state_", metric$id))
      next
    }
    n_ok <- 0
    for (s in STATES) {
      d <- ks[[s$code]]
      if (is.null(d) || nrow(d) < 2) next
      d <- d[d$date >= metric$from, ]
      d$value <- round(d$value, metric$round_digits)
      state_results[[s$code]][[metric$id]] <- list(data = d)
      n_ok <- n_ok + 1
    }
    cat(sprintf("  ✓ %d states from KFF\n", n_ok))
    next
  }

  # FRED-pattern metrics
  n_ok <- 0
  for (s in STATES) {
    fid <- metric$fred_pattern(s)
    res <- tryCatch({
      df <- fetch_fred(fid, from = metric$from)
      if (nrow(df) < 4) stop("too few observations (", nrow(df), ")")
      df$value <- round(df$value, metric$round_digits)
      list(data = df, fred_id = fid)
    }, error = function(e) {
      cat(sprintf("  ✗ %s (%s): %s\n", s$code, fid, conditionMessage(e)))
      failures <<- c(failures, paste0("state_", tolower(s$code), "_", metric$id))
      NULL
    })
    if (!is.null(res)) {
      state_results[[s$code]][[metric$id]] <- res
      n_ok <- n_ok + 1
    }
    Sys.sleep(0.25)
  }
  cat(sprintf("  ✓ %d/%d states\n", n_ok, length(STATES)))
}

# Annual layers
cat("\n── Annual layers ───────────────────────────\n")
annual <- read_annual_layers()

# ── Assemble state payloads ───────────────────────────────────────────────────
today <- format(Sys.Date(), "%Y-%m-%d")

summarize_series <- function(res) {
  df <- res$data
  compact(list(
    latest_value = tail(df$value, 1),
    latest_date  = tail(df$date, 1),
    yoy_change   = { c <- yoy_pct(df); if (!is.na(c)) c else NULL },
    fred_id      = res$fred_id %||% NULL,
    coverage     = res$coverage %||% NULL,
    n_obs        = nrow(df),
    data         = df
  ))
}

# data/states/{code}.js — one state, all metrics
state_names <- setNames(vapply(STATES, function(s) s$name, character(1)),
                        vapply(STATES, function(s) s$code, character(1)))
for (s in STATES) {
  metrics_here <- state_results[[s$code]]
  if (is.null(metrics_here) || length(metrics_here) == 0) next
  payload <- list(
    code    = s$code,
    name    = s$name,
    updated = today,
    metrics = lapply(metrics_here, summarize_series),
    annual  = {
      vals <- list()
      for (m in annual$meta) {
        v <- annual$values[[m$id]][[s$code]]
        if (!is.null(v)) vals[[m$id]] <- v
      }
      vals
    }
  )
  js <- paste0(
    "// Auto-generated by fetch_data.R on ", today, "\n",
    "window.STATE_DATA = window.STATE_DATA || {};\n",
    "window.STATE_DATA[", toJSON(s$code, auto_unbox = TRUE), "] = ",
    toJSON(payload, auto_unbox = TRUE, digits = 6), ";"
  )
  writeLines(js, file.path("data", "states", paste0(tolower(s$code), ".js")))
}

# data/state_metrics/{id}.js — one metric, all states (+ wide CSV)
for (metric in STATE_METRICS) {
  per_state <- list()
  for (s in STATES) {
    res <- state_results[[s$code]][[metric$id]]
    if (is.null(res)) next
    per_state[[s$code]] <- summarize_series(res)
  }
  if (length(per_state) == 0) next

  payload <- list(
    id      = metric$id,
    label   = metric$label,
    units   = metric$units,
    updated = today,
    states  = per_state
  )
  js <- paste0(
    "// Auto-generated by fetch_data.R on ", today, "\n",
    "window.STATE_METRIC = window.STATE_METRIC || {};\n",
    "window.STATE_METRIC[", toJSON(metric$id, auto_unbox = TRUE), "] = ",
    toJSON(payload, auto_unbox = TRUE, digits = 6), ";"
  )
  writeLines(js, file.path("data", "state_metrics", paste0(metric$id, ".js")))

  # Wide CSV: date, one column per state
  merged <- NULL
  for (code in names(per_state)) {
    df <- per_state[[code]]$data
    colnames(df) <- c("date", code)
    merged <- if (is.null(merged)) df else merge(merged, df, by = "date", all = TRUE)
  }
  merged <- merged[order(merged$date), ]
  write.csv(merged, file.path("data", paste0("state_", metric$id, ".csv")), row.names = FALSE)
}

# data/states_index.js — the catalog the front end reads before lazy-loading
metric_index <- lapply(STATE_METRICS, function(m) {
  n_states <- sum(vapply(STATES, function(s)
    !is.null(state_results[[s$code]][[m$id]]), logical(1)))
  list(
    id           = m$id,
    label        = m$label,
    units        = m$units,
    color        = m$color,
    national_id  = m$national_id,
    frequency    = m$frequency,
    source_label = m$source_label,
    description  = m$description,
    invert_color = isTRUE(m$invert_color),
    rebase       = grepl("^Index", m$units),
    n_states     = n_states
  )
})
metric_index <- Filter(function(m) m$n_states > 0, metric_index)

states_index <- list(
  updated = today,
  states  = lapply(STATES, function(s) list(
    code = s$code, name = s$name,
    has  = names(state_results[[s$code]]) %||% character(0)
  )),
  metrics = metric_index,
  annual  = annual$meta
)
writeLines(paste0(
  "// Auto-generated by fetch_data.R on ", today, "\n",
  "window.AFFORDABILITY_STATES = ",
  toJSON(states_index, auto_unbox = TRUE), ";"
), file.path("data", "states_index.js"))

# ── Write national outputs ────────────────────────────────────────────────────

# manifest.json — metadata only (no data arrays), for lightweight inspection
manifest <- list(
  national = lapply(all_data, function(x) { x$data <- NULL; x }),
  states   = list(
    n_states = length(state_results),
    metrics  = metric_index,
    annual   = annual$meta
  )
)
write_json(manifest, "data/manifest.json", auto_unbox = TRUE, pretty = TRUE)

# app_data.js — national data embedded as a JS global, loaded by index.html
js <- paste0(
  "// Auto-generated by fetch_data.R on ", Sys.Date(), "\n",
  "// Re-run `Rscript fetch_data.R` to refresh.\n",
  "window.AFFORDABILITY_DATA = ",
  toJSON(all_data, auto_unbox = TRUE, digits = 6),
  ";"
)
writeLines(js, "data/app_data.js")

n_state_series <- sum(vapply(names(state_results), function(code)
  length(state_results[[code]]), integer(1)))
cat(sprintf(
  "\n─────────────────────────────────────────\n✓ %d national series, %d state series (%d states) written to data/\n  Open index.html in your browser to view.\n─────────────────────────────────────────\n",
  length(all_data), n_state_series, length(state_results)
))

# Fail loudly (non-zero exit) if anything didn't fetch, so the GitHub
# Action's commit step is skipped and the deployed site keeps serving its
# last good data instead of a build silently missing a card. (EIA skipped
# for lack of a key is not a failure; a broken Zillow feed is.)
failed_national <- setdiff(vapply(SERIES, function(x) x$id, character(1)), names(all_data))
failures <- c(failures, failed_national)
if (length(failures) > 0) {
  cat(sprintf("\n✗ %d fetches failed: %s\n", length(failures),
              paste(unique(failures), collapse = ", ")))
  quit(status = 1)
}
