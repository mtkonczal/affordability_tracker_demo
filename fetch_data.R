# fetch_data.R
# Downloads affordability data from FRED and writes to data/
# Run periodically to keep the tracker current.
#
# Usage:  Rscript fetch_data.R
#
# No FRED API key required — uses quantmod's public FRED connector.

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

# ── Series configuration ───────────────────────────────────────────────────────
# To add a new item: append an entry here. The HTML picks it up automatically
# via manifest.json / app_data.js — no HTML edits required.
#
# Fields:
#   id         : slug used for filenames and JS lookups
#   fred_id    : FRED series identifier
#   label      : short display name
#   subtitle   : one-line description shown under the chart title
#   category   : tab grouping ("daily" | "big" | any future category)
#   units      : axis label / tooltip suffix
#   description: longer text shown in tooltip / card header
#   color      : hex color for the chart line
#   from       : earliest data date to fetch

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
  # ── Big Items ──
  list(
    id          = "car_insurance",
    fred_id     = "CUSR0000SETA02",
    label       = "Car Insurance",
    subtitle    = "Motor Vehicle Insurance CPI",
    category    = "big",
    units       = "Index (1982–84 = 100)",
    description = "CPI for motor vehicle insurance. Captures the relative change in auto insurance premiums over time.",
    color       = "#6366F1",
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
    from        = "2000-01-01"
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
    from        = "2000-01-01"
  )
  # ── Add future series below ──
)

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
  cat(sprintf("\nFetching %-20s (%s) ...", cfg$label, cfg$fred_id))

  result <- tryCatch({
    df <- fetch_fred(cfg$fred_id, from = cfg$from)

    # Write per-series CSV (used by the Download button)
    write.csv(df, file.path("data", paste0(cfg$id, ".csv")), row.names = FALSE)

    latest_val  <- tail(df$value, 1)
    latest_date <- tail(df$date,  1)
    change      <- yoy_pct(df)

    entry <- c(
      cfg[c("id", "label", "subtitle", "category", "units", "description", "color", "fred_id")],
      list(
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
