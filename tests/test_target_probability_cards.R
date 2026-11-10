# Presentation-only tests for target-date cards and concise colour emphasis.
source("R/target_probability.R")
source("R/performance_outlook.R")

rtt <- nhs_target_card_html(
  projected = .661982, target = .70,
  target_month = as.Date("2027-03-01"), unit = "proportion", digits = 1L,
  higher_is_better = TRUE, probability = .07
)
stopifnot(grepl("Mar 2027 target outlook", rtt, fixed = TRUE))
stopifnot(grepl("66.2%", rtt, fixed = TRUE))
stopifnot(grepl("&ge;70%", rtt, fixed = TRUE))
stopifnot(grepl("7%", rtt, fixed = TRUE))
stopifnot(grepl("chance-low", rtt, fixed = TRUE))

diagnostics <- nhs_target_card_html(
  projected = .2114147, target = .14,
  target_month = as.Date("2027-03-01"), unit = "proportion", digits = 1L,
  higher_is_better = FALSE, probability = .08
)
stopifnot(grepl("21.1%", diagnostics, fixed = TRUE))
stopifnot(grepl("&le;14%", diagnostics, fixed = TRUE))

ambulance <- nhs_target_card_html(
  projected = 25.396757, target = 25,
  target_month = as.Date("2027-03-01"), unit = "minutes", digits = 1L,
  higher_is_better = FALSE, probability = .49
)
stopifnot(grepl("25.4 min", ambulance, fixed = TRUE))
stopifnot(grepl("&le;25 min", ambulance, fixed = TRUE))
stopifnot(grepl("chance-neutral", ambulance, fixed = TRUE))

stopifnot(nhs_target_chance_class(.005) == "chance-low")
stopifnot(nhs_target_chance_class(.8) == "chance-high")
stopifnot(nhs_target_chance_class(.5) == "chance-neutral")
stopifnot(nhs_target_chance_class(NA_real_) == "chance-unknown")

rtt_row <- data.table::data.table(
  display_order = 1L, overview_group = "Elective care",
  metric_id = "rtt_18w", display_name = "RTT within 18 weeks",
  deep_dive_file = "rtt-18-week-outlook-latest.html",
  expected_direction = "improving", target_label = "70% by March 2027",
  benchmark_label = "70% by March 2027", unit = "proportion", digits = 1L,
  latest_value = .64, latest_month = as.Date("2026-08-01"),
  forecast_month = as.Date("2026-09-01"), forecast_value = .65,
  lower_80 = .63, upper_80 = .67,
  target_month = as.Date("2027-03-01"), target_value = .70,
  projected_target_value = .661982, target_probability = .07,
  higher_is_better = TRUE, edition = "forecast", actual_value = NA_real_
)
overview <- performance_outlook_table_rows(rtt_row)
stopifnot(grepl("milestone-chance chance-low", overview, fixed = TRUE))
stopifnot(grepl("7%", overview, fixed = TRUE))

for (path in c("reports/ae_four_hour_outlook_template.html",
               "reports/core_metric_outlook_template.html")) {
  template <- paste(readLines(path, warn = FALSE), collapse = "\n")
  stopifnot(grepl("{{TARGET_OR_ACTUAL_CARD}}", template, fixed = TRUE))
  stopifnot(grepl(".target-card-chance", template, fixed = TRUE))
}
message("Target-date card and restrained chance colour checks passed.")
