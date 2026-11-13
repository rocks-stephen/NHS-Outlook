# Presentation-only smoke test. Does not touch forecast archives.
source("R/target_probability.R")
source("R/performance_outlook.R")
row <- data.table::data.table(
  display_order = 1L, overview_group = "Elective care",
  metric_id = "rtt_18w", display_name = "RTT within 18 weeks",
  deep_dive_file = "rtt-18-week-outlook-latest.html",
  expected_direction = "improving", target_label = "70% by March 2027",
  benchmark_label = "70% by March 2027", unit = "proportion", digits = 1L,
  latest_value = .64, latest_month = as.Date("2026-09-01"),
  forecast_month = as.Date("2026-10-01"), forecast_value = .65,
  lower_80 = .63, upper_80 = .67,
  target_month = as.Date("2027-03-01"), target_value = .70,
  projected_target_value = .661982, target_probability = .07,
  higher_is_better = TRUE, edition = "forecast", actual_value = NA_real_
)
html <- performance_outlook_table_rows(row)
stopifnot(grepl("Mar 2027 target-date outlook", html, fixed = TRUE))
stopifnot(grepl("66.2%", html, fixed = TRUE))
stopifnot(grepl("&ge;70%", html, fixed = TRUE))
stopifnot(grepl("7%", html, fixed = TRUE))
stopifnot(grepl("model-implied chance", html, fixed = TRUE))
# Lower-is-better: threshold must reverse, without reversing the projection.
row[, `:=`(metric_id = "diagnostics_6w", display_name = "Diagnostics > 6 weeks",
           higher_is_better = FALSE, target_value = .14,
           projected_target_value = .2114147, target_probability = .08)]
html <- performance_outlook_table_rows(row)
stopifnot(grepl("&le;14%", html, fixed = TRUE))
stopifnot(grepl("21.1%", html, fixed = TRUE))
# Confirm that the appropriate headings have been wired into the template.
template <- paste(readLines("reports/nhs_performance_outlook_template.html",
                            warn = FALSE), collapse = "\n")
stopifnot(grepl("{{ACTUAL_HEADING}}", template, fixed = TRUE))
message("Target date/threshold display checks passed")
