# NHS Outlook: Category 2 official national response-time totals.
# Source: NHS England AmbSYS time-series CSV, using A10 (incidents)
# and A30 (total response time in seconds), aggregated over all 11 services.
# Does not invent, impute, or use forecast incident counts for observed months.

nhs_cat2_parse_month <- function(x) {
  z <- trimws(as.character(x))
  n <- suppressWarnings(as.integer(z))
  is_month <- !is.na(n) & n >= 1L & n <= 12L
  n[!is_month] <- match(tolower(substr(z[!is_month], 1L, 3L)),
                        tolower(month.abb))
  n
}

nhs_cat2_as_number <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)),
                                    fixed = TRUE)))
}

# Accept both calendar years ("2026") and financial-year labels ("2026-27"
# or "2026/27"). For FY labels, Jan-Mar belong to the subsequent calendar year.
nhs_cat2_calendar_year <- function(x, month_number) {
  z <- trimws(as.character(x))
  y <- rep(NA_integer_, length(z))
  calendar <- grepl("^[0-9]{4}$", z)
  y[calendar] <- suppressWarnings(as.integer(z[calendar]))
  fiscal <- grepl("^[0-9]{4}[[:space:]]*[-/][[:space:]]*[0-9]{2,4}$", z)
  y[fiscal] <- suppressWarnings(as.integer(substr(z[fiscal], 1L, 4L))) +
    ifelse(!is.na(month_number[fiscal]) & month_number[fiscal] <= 3L, 1L, 0L)
  y
}

nhs_cat2_download_timeseries_csv <- function(
    landing = "https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/") {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("For automatic official AmbSYS CSV download, install xml2: install.packages('xml2'). ",
         "Alternatively set options(nhs.cat2.official_ambsys_csv = 'path/to/AmbSYS.csv').")
  }
  page <- tryCatch(xml2::read_html(landing), error = function(e) {
    stop("Could not read NHS England AQI page: ", conditionMessage(e),
         ". You can download its newest AmbSYS CSV yourself and specify ",
         "options(nhs.cat2.official_ambsys_csv = 'path/to/file.csv').")
  })
  anchors <- xml2::xml_find_all(page, ".//a[@href]")
  labels <- trimws(xml2::xml_text(anchors))
  hrefs <- xml2::xml_attr(anchors, "href")
  valid <- grepl("AmbSYS", labels, ignore.case = TRUE) &
    grepl("[.]csv([?].*)?$", hrefs, ignore.case = TRUE)
  # The current time-series CSV is listed before the older/legacy CSV.
  candidates <- which(valid & !grepl("old ambsys", labels, ignore.case = TRUE))
  if (!length(candidates)) {
    stop("The NHS England page contains no identifiable AmbSYS time-series CSV link. ",
         "Download the latest AmbSYS CSV from the 'CSV data' section and supply its path.")
  }
  href <- hrefs[candidates[1L]]
  url <- if (grepl("^https?://", href, ignore.case = TRUE)) {
    href
  } else if (startsWith(href, "//")) {
    paste0("https:", href)
  } else {
    paste0("https://www.england.nhs.uk/", sub("^/", "", href))
  }
  if (!grepl("^https://(www[.])?england[.]nhs[.]uk/", url)) {
    stop("Refusing a non-NHS England AmbSYS download URL: ", url)
  }
  dir.create("data-external/official", recursive = TRUE, showWarnings = FALSE)
  temp_path <- tempfile("ambsys-", fileext = ".csv")
  on.exit(unlink(temp_path), add = TRUE)
  status <- tryCatch(utils::download.file(url, destfile = temp_path,
                                         mode = "wb", quiet = TRUE),
                     error = function(e) e)
  if (inherits(status, "error") || !file.exists(temp_path) ||
      is.na(file.info(temp_path)$size) || file.info(temp_path)$size < 10000) {
    stop("Could not download official AmbSYS CSV: ", url, ". ",
         "Download it in your browser and set options(nhs.cat2.official_ambsys_csv=...).")
  }
  md5 <- unname(tools::md5sum(temp_path))
  target <- file.path("data-external", "official",
                      paste0("AmbSYS-time-series-", md5, ".csv"))
  if (!file.exists(target) && !file.copy(temp_path, target)) {
    stop("Could not save original AmbSYS CSV to ", target)
  }
  list(path = target, url = url, landing = landing)
}

nhs_cat2_official_csv_source <- function() {
  user_path <- getOption("nhs.cat2.official_ambsys_csv", "")
  if (length(user_path) && nzchar(user_path[1L])) {
    p <- user_path[1L]
    if (!file.exists(p)) stop("The specified official AmbSYS CSV was not found: ", p)
    return(list(path = p,
                url = getOption("nhs.cat2.official_ambsys_url",
                  "https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/"),
                landing = "https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/"))
  }
  nhs_cat2_download_timeseries_csv()
}

nhs_cat2_extract_official_totals <- function(csv_path,
    months = as.Date(sprintf("2026-%02d-01", 4:9))) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table first")
  raw <- data.table::fread(csv_path, encoding = "UTF-8", na.strings = c("", "NA", "."))
  # The NHS England AmbSYS time-series CSV is a provider-month panel with
  # Year, Month, Org Code, Org Name and A10/A30/A31 columns.
  # Convert to lowercase FIRST: otherwise the regex strips capital letters in
  # `Year`, `Month`, `Org Code`, `A10` and `A30` before we match them.
  canonical <- gsub("[^a-z0-9]", "", tolower(names(raw)))
  find_col <- function(name) {
    found <- which(canonical == name)
    if (length(found) != 1L) {
      stop("The selected CSV does not have a unique '", name,
           "' column. Available headers: ", paste(names(raw), collapse = ", "),
           ". Confirm this is the official AmbSYS time-series CSV.")
    }
    names(raw)[found]
  }
  col_y <- find_col("year")
  col_m <- find_col("month")
  col_org <- find_col("orgcode")
  col_a10 <- find_col("a10")
  col_a30 <- find_col("a30")
  col_a31 <- if (sum(canonical == "a31") == 1L) names(raw)[canonical == "a31"] else NULL
  months <- as.Date(months)
  parsed_month <- nhs_cat2_parse_month(raw[[col_m]])
  tab <- data.table::data.table(
    year = nhs_cat2_calendar_year(raw[[col_y]], parsed_month),
    month = parsed_month,
    org_code = trimws(as.character(raw[[col_org]])),
    count = nhs_cat2_as_number(raw[[col_a10]]),
    total_seconds = nhs_cat2_as_number(raw[[col_a30]])
  )
  tab[, calendar_month := as.Date(NA_character_)]
  valid_calendar <- !is.na(tab$year) & !is.na(tab$month) &
    tab$year >= 2017 & tab$year <= 2030
  tab[valid_calendar, calendar_month := as.Date(sprintf(
    "%04d-%02d-01", year, month))]
  tab <- tab[calendar_month %in% months]
  tab <- tab[grepl("^R[A-Z0-9]{2,}$", toupper(org_code))]
  if (!nrow(tab)) {
    stop("The official CSV has no ambulance trust rows for the requested months. ",
         "Check CSV currency (must include September 2026) and Year/Month values. ",
         "Sample Year: ", paste(unique(utils::head(as.character(raw[[col_y]]), 6L)), collapse = ", "),
         "; sample Month: ", paste(unique(utils::head(as.character(raw[[col_m]]), 6L)), collapse = ", "), ".")
  }
  tab[, org_code := toupper(org_code)]
  if (anyDuplicated(tab[, .(calendar_month, org_code)])) {
    stop("Duplicate ambulance trust/month rows found in AmbSYS source; do not aggregate")
  }
  if (any(!is.finite(tab$count) | tab$count <= 0 |
          !is.finite(tab$total_seconds) | tab$total_seconds <= 0)) {
    stop("AmbSYS A10/A30 contains missing, zero or invalid data for included trusts")
  }
  if (!is.null(col_a31)) {
    # Per-service A31 is in seconds; tolerate at most 1 second of rounding.
    raw_month <- nhs_cat2_parse_month(raw[[col_m]])
    raw_year <- nhs_cat2_calendar_year(raw[[col_y]], raw_month)
    raw_dates <- as.Date(rep(NA_character_, length(raw_year)))
    valid_dates <- !is.na(raw_year) & !is.na(raw_month) &
      raw_year >= 2017L & raw_year <= 2030L
    raw_dates[valid_dates] <- as.Date(sprintf(
      "%04d-%02d-01", raw_year[valid_dates], raw_month[valid_dates]))
    raw_match <- raw[
      raw_dates %in% months &
        grepl("^R[A-Z0-9]{2,}$", toupper(trimws(as.character(raw[[col_org]]))))
    ]
    # Do not reject if A31 is formatted as time text rather than seconds.
    a31 <- nhs_cat2_as_number(raw_match[[col_a31]])
    if (length(a31) == nrow(tab) && all(is.finite(a31)) && median(a31) > 100) {
      if (any(abs(a31 - tab$total_seconds / tab$count) > 1.25)) {
        stop("AmbSYS A10/A30 and A31 do not reconcile to within 1.25 seconds")
      }
    }
  }
  out <- tab[, .(
    incident_count = sum(count),
    response_seconds = sum(total_seconds),
    services = data.table::uniqueN(org_code)
  ), by = calendar_month]
  data.table::setorder(out, calendar_month)
  if (!setequal(out$calendar_month, months)) {
    stop("Official AmbSYS CSV missing these months: ",
         paste(setdiff(as.character(months), as.character(out$calendar_month)),
               collapse = ", "), ". Download the latest version of the time-series CSV.")
  }
  if (any(out$services != 11L)) {
    stop("Expected exactly 11 ambulance trusts in each month; observed ",
         paste(paste(out$calendar_month, out$services, sep = ":"), collapse = ", "),
         ". Check revisions and source coverage before publishing.")
  }
  out[, precise_minutes := response_seconds / incident_count / 60]
  out[, published_rounded_seconds := floor(response_seconds / incident_count + 0.5)]
  out
}

nhs_cat2_backfill_national <- function(
    panel_path = "data-interim/core/national_panel.csv",
    backfill_months = as.Date(sprintf("2026-%02d-01", 4:9)),
    audit_path = "output/qa/ambulance_cat2_official_backfill.csv") {
  source <- nhs_cat2_official_csv_source()
  # April--September must come from the CSV. October is already independently
  # checked against the original NHS England monthly workbook (A10/A30).
  month_check <- as.Date(sprintf("2026-%02d-01", 4:9))
  totals <- nhs_cat2_extract_official_totals(source$path, month_check)
  panel <- data.table::fread(panel_path, encoding = "UTF-8")
  panel[, calendar_month := as.Date(calendar_month)]
  required <- c("metric_id", "calendar_month", "value", "numerator", "denominator")
  if (!all(required %in% names(panel))) stop("Missing panel columns: ",
                                            paste(setdiff(required, names(panel)), collapse = ", "))
  current <- panel[metric_id == "ambulance_cat2" &
                     calendar_month %in% month_check,
                   .(calendar_month, prior_mean_minutes = value,
                     prior_incidents = denominator)]
  checked <- merge(totals, current, by = "calendar_month", all.x = TRUE)
  if (nrow(checked) != 6L || anyNA(checked$prior_mean_minutes)) {
    stop("One or more official April–October months are absent from the existing national panel")
  }
  checked[, disagreement_seconds :=
            abs(precise_minutes - prior_mean_minutes) * 60]
  # Existing values are rounded to whole seconds. That alone can account for
  # at most half a second's discrepancy. Allow a 0.65s tolerance for floating point.
  if (any(checked$disagreement_seconds > 0.65)) {
    print(checked[, .(calendar_month, prior_mean_minutes,
                      precise_minutes, disagreement_seconds)])
    stop("Published AmbSYS A10/A30 totals differ from existing monthly Cat2 means ",
         "by more than rounding allows. Resolve the source revision BEFORE ",
         "overwriting any values or regenerating the forecast.")
  }
  # October was already reconciled directly against the uploaded NHS workbook.
  # If the refreshed CSV contains October, add an additional independent check.
  october_csv <- tryCatch(
    nhs_cat2_extract_official_totals(source$path, as.Date("2026-10-01")),
    error = function(e) NULL)
  if (!is.null(october_csv) && nrow(october_csv) == 1L) {
    october_panel <- panel[metric_id == "ambulance_cat2" &
                              calendar_month == as.Date("2026-10-01")]
    if (nrow(october_panel) != 1L ||
        !is.finite(october_panel$denominator[1L]) ||
        abs(october_panel$denominator[1L] - october_csv$incident_count[1L]) > 0.5 ||
        abs(october_panel$value[1L] - october_csv$precise_minutes[1L]) * 60 > 0.65) {
      stop("AmbSYS CSV and the independently validated October workbook differ; " ,
           "check whether the monthly data were revised.")
    }
  }
  # Keep a separate official history for the rolling 12-month chart, without
  # rewriting previous-year model inputs or changing their release vintage.
  historical_months <- seq.Date(as.Date("2025-01-01"),
                                 as.Date("2026-09-01"), by = "month")
  official_history <- nhs_cat2_extract_official_totals(source$path,
                                                        historical_months)
  prior_history <- panel[metric_id == "ambulance_cat2" &
                          calendar_month %in% historical_months,
                        .(calendar_month, panel_minutes = value)]
  historical_review <- merge(official_history, prior_history,
                             by = "calendar_month", all.x = TRUE)
  if (anyNA(historical_review$panel_minutes) ||
      any(abs(historical_review$panel_minutes -
              historical_review$precise_minutes) * 60 > 0.65)) {
    print(historical_review[, .(calendar_month, panel_minutes, precise_minutes)])
    stop("The AmbSYS official historic CSV conflicts with previously imported " ,
         "response-time means. Check revision notes before adding a rolling chart.")
  }
  dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(official_history,
                     "output/qa/ambulance_cat2_official_history.csv")
  backfill_months <- as.Date(backfill_months)
  changed <- panel[metric_id == "ambulance_cat2" &
                     calendar_month %in% backfill_months]
  if (nrow(changed) != length(backfill_months)) {
    stop("Cannot backfill: the requested panel months are not unique and complete")
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Need digest for official-source SHA-256 provenance: install.packages('digest')")
  }
  source_sha <- digest::digest(source$path, algo = "sha256", file = TRUE)
  # Validate all observations BEFORE mutating or writing the panel.
  for (k in seq_len(nrow(checked))) {
    m <- checked$calendar_month[k]
    ix <- which(panel$metric_id == "ambulance_cat2" & panel$calendar_month == m)
    if (length(ix) != 1L) stop("Duplicate or missing Cat2 row: ", m)
  }
  orig_panel <- panel_path
  dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
  backup <- file.path("output", "qa", "national_panel_before_cat2_backfill.csv")
  if (!file.exists(backup)) {
    if (!file.copy(orig_panel, backup, overwrite = FALSE)) {
      stop("Cannot create original-panel backup: ", backup)
    }
  }
  # A30 is TOTAL SECONDS. Convert to TOTAL MINUTES in numerator and
  # keep the precise A30/A10 ratio as value so structural QA reconciles.
  for (k in seq_len(nrow(checked))) {
    m <- checked$calendar_month[k]
    if (!(m %in% backfill_months)) next
    ix <- which(panel$metric_id == "ambulance_cat2" & panel$calendar_month == m)
    data.table::set(panel, i = ix, j = "numerator",
                    value = checked$response_seconds[k] / 60)
    data.table::set(panel, i = ix, j = "denominator",
                    value = checked$incident_count[k])
    data.table::set(panel, i = ix, j = "value",
                    value = checked$precise_minutes[k])
    if ("source_file" %in% names(panel)) {
      data.table::set(panel, i = ix, j = "source_file",
                      value = normalizePath(source$path, winslash = "/", mustWork = TRUE))
    }
    if ("source_sha256" %in% names(panel)) {
      data.table::set(panel, i = ix, j = "source_sha256", value = source_sha)
    }
    if ("source_url" %in% names(panel) && nzchar(source$url)) {
      data.table::set(panel, i = ix, j = "source_url", value = source$url)
    }
    if ("source_method" %in% names(panel)) {
      data.table::set(panel, i = ix, j = "source_method",
                      value = "official_ambsys_csv_A10_A30")
    }
  }
  checked[, `:=`(source_csv = normalizePath(source$path, winslash = "/",
                                             mustWork = TRUE),
                 source_url = source$url,
                 source_sha256 = source_sha,
                 applied = calendar_month %in% backfill_months)]
  data.table::fwrite(checked, audit_path)
  data.table::fwrite(panel, panel_path)
  message("Category 2 official backfill complete for ", length(backfill_months),
          " months. Full source audit: ", audit_path,
          ". Refit core metrics (script 15) to update projections consistently.")
  invisible(checked)
}
