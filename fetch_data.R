# fetch_data.R
# Downloads affordability data from FRED and writes to data/
# Run periodically to keep the tracker current.
#
# Usage:  Rscript fetch_data.R
#
# No FRED API key required — uses quantmod's public FRED connector.

# Some shells (this machine's default, some CI runners) start R in the "C"
# locale, under which non-ASCII characters (en dashes, ÷) get written out as
# literal "<c3><b7>"-style escapes instead of real UTF-8 bytes. Force a UTF-8
# locale so labels/units render correctly regardless of the invoking shell.
for (loc in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
}

# ── Package bootstrap ──────────────────────────────────────────────────────────
required <- c("jsonlite")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(pkg, repos = "https://cran.r-project.org/")
  }
}
suppressMessages(library(jsonlite))

# Fetch a FRED series as a data.frame using the public CSV endpoint (no API key)
fetch_fred <- function(fred_id, from = "2000-01-01") {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", fred_id)
  df  <- tryCatch(
    read.csv(url(url), stringsAsFactors = FALSE),
    error = function(e) stop("HTTP fetch failed: ", conditionMessage(e))
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

# ── Series configuration ───────────────────────────────────────────────────────
# To add a new item: append an entry here. The HTML picks it up automatically
# via manifest.json / app_data.js — no HTML edits required.
#
# Fields:
#   id           : slug used for filenames and JS lookups
#   fred_id      : FRED series identifier
#   label        : short display name
#   subtitle     : one-line description shown under the chart title
#   category     : tab grouping ("daily" | "big" | "labor" | "debt" | "state" | any future category)
#   units        : axis label / tooltip suffix
#   description  : longer text shown in tooltip / card header
#   color        : hex color for the chart line
#   from         : earliest data date to fetch
#   geo          : optional — postal-code-style geography, e.g. "CA" for a state series (default "us")
#   is_new       : optional — TRUE flags a series for the "Newest" filter (version-tracking)
#   invert_color : optional — TRUE means a rising value is good news (renders green, not red);
#                  used for wage/labor-demand series where "up" isn't bad
#   scale        : optional — multiplier applied to fetched values (e.g. 0.001 to show
#                  millions-of-dollars series in billions for readability)

SERIES <- list(
  # ── Daily Items ──
  list(
    id          = "groceries",
    fred_id     = "CPIFABSL",
    label       = "Groceries",
    subtitle    = "Food at Home CPI",
    category    = "daily",
    units       = "Index (1982–84 = 100)",
    description = "CPI for all urban consumers: food at home. Tracks how much grocery prices have risen relative to a 1982–84 baseline.",
    color       = "#F97316",
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
    color       = "#EAB308",
    from        = "2000-01-01"
  ),
  list(
    id          = "eggs",
    fred_id     = "APU0000708111",
    label       = "Eggs",
    subtitle    = "Grade A, Large, per Dozen",
    category    = "groceries",
    units       = "$ per Dozen",
    description = "Average retail price of Grade A large eggs in U.S. city averages, from the BLS Average Price data.",
    color       = "#FACC15",
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
    color       = "#B45309",
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
    color       = "#FCA5A5",
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
    color       = "#93C5FD",
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
    color       = "#FDE047",
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
    color       = "#A16207",
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
    color       = "#F59E0B",
    from        = "2000-01-01"
  ),
  # ── Big Items ──
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
    color       = "#8B5CF6",
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
    color       = "#14B8A6",
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
    color       = "#F472B6",
    from        = "2000-01-01"
  ),
  list(
    id          = "rent",
    fred_id     = "CUUR0000SEHA",
    label       = "Rent",
    subtitle    = "Rent of Primary Residence CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for rent of primary residence, all urban consumers, not seasonally adjusted.",
    color       = "#0EA5E9",
    from        = "2000-01-01"
  ),
  list(
    id          = "median_home_price",
    fred_id     = "MSPUS",
    label       = "Median Home Price",
    subtitle    = "Median Sales Price, Houses Sold (US)",
    category    = "big",
    units       = "$",
    description = "Median sales price of houses sold in the United States. Quarterly, from the Census Bureau / HUD.",
    color       = "#DC2626",
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
    color       = "#EC4899",
    from        = "2000-01-01"
  ),
  # ── Labor Market ──
  list(
    id          = "unemployment",
    fred_id     = "UNRATE",
    label       = "Unemployment Rate",
    subtitle    = "US Civilian Unemployment",
    category    = "labor",
    units       = "Rate (%)",
    description = "Percent of the labor force that is unemployed and actively seeking work. Monthly, seasonally adjusted.",
    color       = "#0EA5E9",
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
    color       = "#8B5CF6",
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
    color       = "#0891B2",
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
    color       = "#7C3AED",
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
    color       = "#A855F7",
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
    from        = "2000-01-01",
    is_new      = TRUE
  ),
  # ── Debt (Plan 3) ──
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
    scale       = 0.001,
    is_new      = TRUE
  ),
  list(
    id          = "student_loans",
    fred_id     = "SLOASM",
    label       = "Student Loans",
    subtitle    = "Student Loan Debt Outstanding",
    category    = "debt",
    units       = "$ Billions",
    description = "Total student loan debt owned and securitized. Federal Reserve G.19 release.",
    color       = "#7C3AED",
    from        = "2006-01-01",
    scale       = 0.001,
    is_new      = TRUE
  ),
  list(
    id          = "auto_loans",
    fred_id     = "MVLOASM",
    label       = "Auto Loans",
    subtitle    = "Motor Vehicle Loan Debt Outstanding",
    category    = "debt",
    units       = "$ Billions",
    description = "Total motor vehicle loan debt owned and securitized. Federal Reserve G.19 release.",
    color       = "#2563EB",
    from        = "2000-01-01",
    scale       = 0.001,
    is_new      = TRUE
  ),
  list(
    id          = "credit_card_delinquency",
    fred_id     = "DRCCLACBS",
    label       = "Credit Card Delinquency",
    subtitle    = "Delinquency Rate, All Commercial Banks",
    category    = "debt",
    units       = "Rate (%)",
    description = "Share of credit card loan balances 30+ days delinquent at all commercial banks. Quarterly, seasonally adjusted.",
    color       = "#EA580C",
    from        = "2000-01-01",
    is_new      = TRUE
  ),
  list(
    id          = "consumer_loan_delinquency",
    fred_id     = "DRCLACBS",
    label       = "Consumer Loan Delinquency",
    subtitle    = "Delinquency Rate, All Commercial Banks",
    category    = "debt",
    units       = "Rate (%)",
    description = "Share of consumer loan balances 30+ days delinquent at all commercial banks. Quarterly, seasonally adjusted.",
    color       = "#F97316",
    from        = "2000-01-01",
    is_new      = TRUE
  ),
  list(
    id          = "mortgage_delinquency",
    fred_id     = "DRSFRMACBS",
    label       = "Mortgage Delinquency",
    subtitle    = "Single-Family Residential, All Commercial Banks",
    category    = "debt",
    units       = "Rate (%)",
    description = "Share of single-family residential mortgage balances 30+ days delinquent at all commercial banks. Quarterly, seasonally adjusted.",
    color       = "#B91C1C",
    from        = "2000-01-01",
    is_new      = TRUE
  )
  # ── Add future series below ──
)

# ── State pilot (Plan 2) ─────────────────────────────────────────────────────
# Five states x four core series, chosen for reliable FRED coverage under a
# consistent ID pattern (verified against fredgraph.csv before adding):
#   unemployment  — state LAUS unemployment rate ({STATE}UR)
#   wages         — state CES avg hourly earnings, total private (SMU...003)
#   home_prices   — FHFA All-Transactions House Price Index ({STATE}STHPI)
#   income        — Real (CPI-U-RS-adjusted) median household income (ACS)
# No state-level CPI exists, so this pilot deliberately avoids implying one.
STATES <- list(
  list(code = "CA", name = "California",   uer = "CAUR", hpi = "CASTHPI", inc = "MEHOINUSCAA672N", wage = "SMU06000000500000003"),
  list(code = "TX", name = "Texas",        uer = "TXUR", hpi = "TXSTHPI", inc = "MEHOINUSTXA672N", wage = "SMU48000000500000003"),
  list(code = "FL", name = "Florida",      uer = "FLUR", hpi = "FLSTHPI", inc = "MEHOINUSFLA672N", wage = "SMU12000000500000003"),
  list(code = "OH", name = "Ohio",         uer = "OHUR", hpi = "OHSTHPI", inc = "MEHOINUSOHA672N", wage = "SMU39000000500000003"),
  list(code = "PA", name = "Pennsylvania", uer = "PAUR", hpi = "PASTHPI", inc = "MEHOINUSPAA672N", wage = "SMU42000000500000003")
)

state_series <- do.call(c, lapply(STATES, function(s) list(
  list(
    id          = paste0("state_", tolower(s$code), "_unemployment"),
    fred_id     = s$uer,
    label       = paste0(s$code, " Unemployment"),
    subtitle    = paste0(s$name, " Unemployment Rate (LAUS)"),
    category    = "state",
    units       = "Rate (%)",
    description = paste0("Unemployment rate for ", s$name, ", seasonally adjusted. From the BLS Local Area Unemployment Statistics program."),
    color       = "#0EA5E9",
    from        = "2000-01-01",
    geo         = s$code,
    is_new      = TRUE
  ),
  list(
    id          = paste0("state_", tolower(s$code), "_wages"),
    fred_id     = s$wage,
    label       = paste0(s$code, " Wages"),
    subtitle    = paste0(s$name, " Avg Hourly Earnings"),
    category    = "state",
    units       = "$ per Hour",
    description = paste0("Average hourly earnings of all employees, total private, in ", s$name, ". From the BLS state Current Employment Statistics (CES) program, not seasonally adjusted."),
    color       = "#10B981",
    from        = "2007-01-01",
    geo         = s$code,
    invert_color = TRUE,
    is_new      = TRUE
  ),
  list(
    id          = paste0("state_", tolower(s$code), "_home_prices"),
    fred_id     = s$hpi,
    label       = paste0(s$code, " Home Prices"),
    subtitle    = paste0(s$name, " House Price Index"),
    category    = "state",
    units       = "Index (1980 Q1 = 100)",
    description = paste0("FHFA All-Transactions House Price Index for ", s$name, ", quarterly, not seasonally adjusted."),
    color       = "#DC2626",
    from        = "2000-01-01",
    geo         = s$code,
    is_new      = TRUE
  ),
  list(
    id          = paste0("state_", tolower(s$code), "_income"),
    fred_id     = s$inc,
    label       = paste0(s$code, " Median Income"),
    subtitle    = paste0(s$name, " Real Median Household Income"),
    category    = "state",
    units       = "$",
    description = paste0("Real (CPI-U-RS-adjusted) median household income in ", s$name, ", annual, from the Census Bureau American Community Survey."),
    color       = "#8B5CF6",
    from        = "2000-01-01",
    geo         = s$code,
    invert_color = TRUE,
    is_new      = TRUE
  )
)))

SERIES <- c(SERIES, state_series)

# ── Helpers ───────────────────────────────────────────────────────────────────
yoy_pct <- function(df) {
  latest      <- tail(df, 1)
  cutoff      <- as.Date(latest$date) - 365
  prior_rows  <- df[as.Date(df$date) <= cutoff, ]
  if (nrow(prior_rows) == 0) return(NA_real_)
  prior_val   <- tail(prior_rows$value, 1)
  round((latest$value - prior_val) / prior_val * 100, 2)
}

# ── Download loop ─────────────────────────────────────────────────────────────
dir.create("data", showWarnings = FALSE, recursive = TRUE)
all_data <- list()

for (cfg in SERIES) {
  src_id <- if (!is.null(cfg$source) && cfg$source == "bls") cfg$bls_id else cfg$fred_id
  cat(sprintf("\nFetching %-20s (%s) ...", cfg$label, src_id))

  result <- tryCatch({
    df <- if (!is.null(cfg$source) && cfg$source == "bls") {
      fetch_bls(cfg$bls_id, from = cfg$from)
    } else {
      fetch_fred(cfg$fred_id, from = cfg$from)
    }

    # Optional unit-conversion multiplier (e.g. 0.001 to show a
    # millions-of-dollars series in billions). Purely a display scale —
    # doesn't touch the underlying FRED values' meaning.
    if (!is.null(cfg$scale)) df$value <- df$value * cfg$scale

    # Write per-series CSV (used by the Download button) — raw levels, since
    # the front end will rebase index series at view-time based on the
    # selected x-axis range.
    write.csv(df, file.path("data", paste0(cfg$id, ".csv")), row.names = FALSE)

    latest_val  <- tail(df$value, 1)
    latest_date <- tail(df$date,  1)
    change      <- yoy_pct(df)

    # `rebase = TRUE` tells the front end: don't display the raw index value;
    # show cumulative % change since the start of the visible x-axis window.
    rebase_flag <- grepl("^Index", cfg$units)

    entry <- c(
      cfg[c("id", "label", "subtitle", "category", "units", "description", "color", "fred_id")],
      list(
        geo          = if (!is.null(cfg$geo)) cfg$geo else "us",
        is_new       = isTRUE(cfg$is_new),
        invert_color = isTRUE(cfg$invert_color),
        rebase       = rebase_flag,
        last_updated = format(Sys.Date(), "%Y-%m-%d"),
        latest_value = round(latest_val, 3),
        latest_date  = latest_date,
        yoy_change   = if (!is.na(change)) change else NULL,
        n_obs        = nrow(df),
        data         = df
      )
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
    geo          = "us",
    is_new       = TRUE,
    invert_color = TRUE,
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

# ── Write outputs ─────────────────────────────────────────────────────────────

# manifest.json — metadata only (no data arrays), for lightweight inspection
manifest <- lapply(all_data, function(x) { x$data <- NULL; x })
write_json(manifest, "data/manifest.json", auto_unbox = TRUE, pretty = TRUE)

# app_data.js — full data embedded as a JS global, loaded by index.html
js <- paste0(
  "// Auto-generated by fetch_data.R on ", Sys.Date(), "\n",
  "// Re-run `Rscript fetch_data.R` to refresh.\n",
  "window.AFFORDABILITY_DATA = ",
  toJSON(all_data, auto_unbox = TRUE, digits = 6),
  ";"
)
writeLines(js, "data/app_data.js")

cat(sprintf(
  "\n─────────────────────────────────────────\n✓ %d series written to data/\n  Open index.html in your browser to view.\n─────────────────────────────────────────\n",
  length(all_data)
))

# Fail loudly (non-zero exit) if any series didn't fetch, so the GitHub
# Action's commit step is skipped and the deployed site keeps serving its
# last good data instead of a build silently missing a card.
failed_ids <- setdiff(vapply(SERIES, function(x) x$id, character(1)), names(all_data))
if (length(failed_ids) > 0) {
  cat(sprintf("\n✗ %d series failed to fetch: %s\n", length(failed_ids), paste(failed_ids, collapse = ", ")))
  quit(status = 1)
}
