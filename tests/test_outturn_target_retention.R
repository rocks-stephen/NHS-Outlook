# Presentation-only checks: keep actual/forecast AND the issued target-date outlook.
source("R/target_probability.R")
source("R/performance_outlook.R")

row <- data.table::data.table(
  display_order = 1L, overview_group = "Planned care",
  metric_id = "rtt_18w", display_name = "RTT within 18 weeks",
  deep_dive_file = "rtt-18-week-outturn-latest.html",
  expected_direction = "broadly_stable", target_label = "70% by March 2027",
  benchmark_label = "92% constitutional standard", unit = "proportion", digits = 1L,
  latest_value = .652, latest_month = as.Date("2026-08-01"),
  forecast_month = as.Date("2026-09-01"), forecast_value = .651,
  lower_80 = .639, upper_80 = .663,
  target_month = as.Date("2027-03-01"), target_value = .70,
  projected_target_value = .661982, target_probability = .07,
  higher_is_better = TRUE, edition = "outturn", actual_month = as.Date("2026-09-01"),
  actual_value = .654, actual_error_native = .003,
  previous_value = .652, flat_threshold_native = .001
)
# Reuse the real report renderer to verify the outturn has both comparisons.
out <- performance_outlook_table_rows(row)
stopifnot(grepl("65.1%", out, fixed=TRUE))
stopifnot(grepl("65.4%", out, fixed=TRUE))
stopifnot(grepl("66.2%", out, fixed=TRUE))
stopifnot(grepl("&ge;70%", out, fixed=TRUE))
stopifnot(grepl("7%", out, fixed=TRUE))
stopifnot(length(regmatches(out, gregexpr('<td ', out, fixed=TRUE))[[1]]) == 3L)
strip <- nhs_target_strip_html(.661982, .70, as.Date("2027-03-01"),
                               "proportion", 1L, TRUE, .07)
stopifnot(grepl("outlook at issue", strip, fixed=TRUE))
stopifnot(grepl("66.2%", strip, fixed=TRUE))
stopifnot(grepl("&ge;70%", strip, fixed=TRUE))
stopifnot(grepl("7%", strip, fixed=TRUE))
for (path in c("reports/ae_four_hour_outlook_template.html",
               "reports/core_metric_outlook_template.html")) {
  template <- paste(readLines(path, warn=FALSE),collapse="\n")
  stopifnot(grepl("{{TARGET_DATE_STRIP}}", template, fixed=TRUE))
  stopifnot(grepl(".target-outturn-strip", template, fixed=TRUE))
}
message("Outturn retains forecast-versus-actual and issued target-date chance.")
