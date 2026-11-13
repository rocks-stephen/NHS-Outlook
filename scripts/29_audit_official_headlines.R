# Independent headline control against the published NHS statistics.
# Do NOT label an indicator verified until its published national figure has
# actually been checked. Four results initially require manual confirmation.
source("R/ambulance_official.R")
if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table is required")

bench <- data.table::fread("config/official_headline_reference_2026-11-12.csv",
                           encoding = "UTF-8")
report <- data.table::fread("output/performance/outturn_metric_rows.csv",
                            encoding = "UTF-8")
stopifnot(nrow(bench) == 8L, !anyDuplicated(bench$metric_id),
          !anyDuplicated(report$metric_id))
if (!all(c("metric_id", "calendar_month", "official_value", "official_unit",
           "verification_status", "source_or_check") %in% names(bench))) {
  stop("Reference file is missing required columns.")
}
if (!"actual_value" %in% names(report)) {
  stop("Outturn metric rows do not contain 'actual_value'.")
}

actual <- report[, .(metric_id, actual_value,
                     actual_month = as.character(actual_month))]
comparison <- merge(bench, actual, by = "metric_id", all.x = TRUE,
                    sort = FALSE)
comparison[, month_correct := as.character(calendar_month) == as.character(actual_month)]
comparison[, `:=`(actual_as_published = NA_character_,
                  official_as_published = NA_character_,
                  matches_published = FALSE)]
for (i in seq_len(nrow(comparison))) {
  x <- comparison$actual_value[i]
  ref <- comparison$official_value[i]
  if (!is.finite(x) || !is.finite(ref) ||
      !isTRUE(comparison$month_correct[i])) next
  if (comparison$official_unit[i] == "seconds") {
    comparison$actual_as_published[i] <- nhs_cat2_clock(x)
    comparison$official_as_published[i] <- nhs_cat2_clock(ref / 60)
    comparison$matches_published[i] <- as.integer(round(x * 60)) ==
      as.integer(round(ref))
  } else if (comparison$official_unit[i] == "proportion") {
    # Both series are checked at the 1-decimal-percentage-point precision
    # actually visible in the official statistical headline.
    if (x > 1) x <- x / 100
    comparison$actual_as_published[i] <- sprintf("%.1f%%", 100 * x)
    comparison$official_as_published[i] <- sprintf("%.1f%%", 100 * ref)
    comparison$matches_published[i] <- identical(
      comparison$actual_as_published[i],
      comparison$official_as_published[i]
    )
  } else {
    stop("Unknown official_unit for ", comparison$metric_id[i])
  }
}
comparison[, result := data.table::fifelse(
  !month_correct | !matches_published, "MISMATCH",
  data.table::fifelse(verification_status == "verified", "VERIFIED_MATCH",
                       "SOURCE_NOT_YET_VERIFIED")
)]
dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(comparison, "output/qa/national_headline_source_audit.csv")
print(comparison[, .(metric_id, calendar_month, actual_as_published,
                     official_as_published, result, source_or_check)])
if (any(comparison$result == "MISMATCH")) {
  stop("Published headline mismatch. Do not publish; review QA CSV.")
}
# The two public headline displays must show the NHS England clock value,
# not a one-decimal-minute approximation of it.
cat2_row <- comparison[metric_id == "ambulance_cat2"]
for (html_file in c("output/releases/nhs-performance-outturn-latest.html",
                    "output/releases/ambulance-category-2-outturn-latest.html")) {
  if (!file.exists(html_file)) stop("Missing output HTML: ", html_file)
  html <- paste(readLines(html_file, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  if (!grepl(cat2_row$official_as_published[1L], html, fixed = TRUE)) {
    stop("Ambulance headline still differs from the official clock in: ", html_file)
  }
}

if (any(comparison$result == "SOURCE_NOT_YET_VERIFIED")) {
  message("Four source figures need independent confirmation. ",
          "Do not claim a complete eight-indicator source audit yet.")
  if (isTRUE(getOption("nhs.outlook.require_all_official_signoff", TRUE))) {
    stop("Pre-publication headline audit incomplete: confirm remaining official ",
         "national source rows and change verification_status to verified only ",
         "after checking the original NHS publications.")
  }
}
message("National headline reconciliation finished. See output/qa/national_headline_source_audit.csv")
