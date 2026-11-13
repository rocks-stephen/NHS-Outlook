# NHS Outlook: official AmbSYS national Category 2 response-time control.
# Modelling remains in continuous decimal minutes. Published observed values are
# displayed as HH/MM/SS (minutes and seconds) so they match the NHS England file.
# The reported mean is rounded to whole seconds in AmbSYS. The more precise
# analytical mean is Total response hours * 60 / incident count.

nhs_ambsys_read_national_cat2 <- function(path, month) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install readxl first: install.packages('readxl')")
  }
  if (!file.exists(path)) stop("Missing approved AmbSYS workbook: ", path)
  # Fail closed if the published worksheet changes: no silent use of a wrong row.
  section <- readxl::read_excel(path, sheet = "Response Times",
                                range = "B34", col_names = FALSE,
                                col_types = "text")[[1L]][1L]
  if (!identical(trimws(as.character(section)), "Category 2")) {
    stop("AmbSYS layout changed: B34 is not 'Category 2'. Inspect: ", path)
  }
  z <- readxl::read_excel(path, sheet = "Response Times",
                          range = "D35:H35", col_names = FALSE,
                          col_types = c("text", "numeric", "skip", "numeric", "numeric"))
  if (nrow(z) != 1L || ncol(z) != 4L ||
      !identical(trimws(as.character(z[[1L]][1L])), "England")) {
    stop("AmbSYS D35:H35 no longer contains the England Category 2 row.")
  }
  count <- as.numeric(z[[2L]][1L])  # E35 incidents
  hours <- as.numeric(z[[3L]][1L])  # G35 response-time hours
  time_fraction <- as.numeric(z[[4L]][1L])  # H35 Excel-day fraction, HH:MM:SS
  if (!is.finite(count) || !is.finite(hours) || !is.finite(time_fraction) ||
      count <= 0 || hours <= 0 || time_fraction <= 0 || time_fraction >= 1) {
    stop("Invalid England C2 totals or mean in: ", path)
  }
  published_seconds <- as.integer(round(time_fraction * 86400))
  derived_minutes <- (hours * 60) / count
  discrepancy_seconds <- derived_minutes * 60 - published_seconds
  if (abs(discrepancy_seconds) > 0.55) {
    stop("Published C2 mean does not reconcile with response hours and incidents: ",
         round(discrepancy_seconds, 3), " seconds. Inspect the NHS workbook.")
  }
  data.table::data.table(
    metric_id = "ambulance_cat2", calendar_month = data.table::as.IDate(month),
    source_file = normalizePath(path, winslash = "/", mustWork = TRUE),
    incidents = count, total_response_hours = hours,
    published_seconds = published_seconds,
    published_minutes = published_seconds / 60,
    precise_minutes = derived_minutes,
    rounded_mean_difference_seconds = discrepancy_seconds
  )
}

nhs_cat2_clock <- function(minutes, style = "published") {
  if (length(minutes) != 1L || !is.finite(minutes) || minutes < 0) return("—")
  seconds <- as.integer(floor(minutes * 60 + 0.50000001))
  mins <- seconds %/% 60L
  sec <- seconds %% 60L
  if (identical(style, "compact")) sprintf("%dm %02ds", mins, sec) else
    sprintf("%d min %02d sec", mins, sec)
}

nhs_cat2_read_control <- function(config_path = "config/ambsys_official_sources.csv") {
  if (!file.exists(config_path)) stop("Missing ", config_path)
  cfg <- data.table::fread(config_path, encoding = "UTF-8")
  if (!all(c("calendar_month", "workbook") %in% names(cfg))) {
    stop("Expected calendar_month and workbook in: ", config_path)
  }
  cfg[, calendar_month := data.table::as.IDate(as.character(calendar_month))]
  if (!nrow(cfg) || anyDuplicated(cfg$calendar_month)) {
    stop("AmbSYS approved source list is empty or duplicates calendar months.")
  }
  data.table::rbindlist(lapply(seq_len(nrow(cfg)), function(i) {
    nhs_ambsys_read_national_cat2(cfg$workbook[i], cfg$calendar_month[i])
  }))
}

nhs_cat2_validate_model_values <- function(panel, reference, tolerance_seconds = 3.6) {
  rows <- panel[metric_id == "ambulance_cat2"]
  merged <- merge(reference, rows, by = c("metric_id", "calendar_month"),
                  all.x = TRUE, suffixes = c("_official", "_model"))
  if (nrow(merged) != nrow(reference) || anyNA(merged$value)) {
    stop("Cannot reconcile AmbSYS source: selected month absent from national panel.")
  }
  merged[, discrepancy_seconds := (value - precise_minutes) * 60]
  # Importers which round to one decimal minute can differ by up to 3 seconds;
  # larger discrepancies suggest a different measure/month and must not be patched.
  if (any(abs(merged$discrepancy_seconds) > tolerance_seconds)) {
    stop("Cat2 importer differs materially from original AmbSYS totals: ",
         paste(merged$calendar_month[abs(merged$discrepancy_seconds) > tolerance_seconds],
               collapse = ", "))
  }
  merged
}
