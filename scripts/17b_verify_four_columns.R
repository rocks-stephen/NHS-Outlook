# Run after scripts/17_build_performance_outlook.R in outturn mode.
# This checks all eight latest probabilities were propagated from fresh modelling.
source("R/utils.R")
stopifnot(nhs_outlook_publication_mode() == "outturn")

fresh <- data.table::fread("output/performance/overview_metric_rows.csv")
outturn <- data.table::fread("output/performance/outturn_metric_rows.csv")
stopifnot(nrow(outturn) == 8L, !anyDuplicated(outturn$metric_id))
stopifnot(all(c("metric_id", "target_probability", "projected_target_value") %in%
              names(fresh)))
stopifnot(all(c("metric_id", "target_probability", "projected_target_value",
                "at_forecast_target_probability", "outlook_refreshed_at_issue") %in%
              names(outturn)))
i <- match(outturn$metric_id, fresh$metric_id)
stopifnot(!anyNA(i))
stopifnot(isTRUE(all.equal(outturn$target_probability,
                           fresh$target_probability[i], tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(outturn$projected_target_value,
                           fresh$projected_target_value[i], tolerance = 1e-12)))
# Assert that every original probability is still exactly preserved.
snapshot <- data.table::fread("output/performance/latest_published_forecast_rows.csv")
j <- match(outturn$metric_id, snapshot$metric_id)
stopifnot(!anyNA(j))
stopifnot(isTRUE(all.equal(outturn$at_forecast_target_probability,
                           snapshot$target_probability[j], tolerance = 1e-12)))

page <- file.path("output/releases", "nhs-performance-outturn-latest.html")
stopifnot(file.exists(page))
html <- paste(readLines(page, warn = FALSE), collapse = "\n")
required <- c("Previous", "Forecast", "Actual",
              "Target outlook", "chance of meeting goal",
              "outlook-four-column-table", "forecast-cell",
              "range-badge", "signal-good", "signal-bad",
              "probability-change", "since forecast")
stopifnot(all(vapply(required, grepl, logical(1), x = html, fixed = TRUE)))
# One commentary line per indicator, not just CSS class declarations.
commentary_count <- lengths(regmatches(
  html, gregexpr('class="probability-change ', html, fixed = TRUE)
))
stopifnot(identical(as.integer(commentary_count), 8L))
cat("PASS: Eight freshly recalculated probabilities in outturn rows; ",
    "four outturn data columns found in HTML.\n", sep = "")
print(outturn[, .(
  metric_id,
  probability_at_forecast_pct = round(100 * at_forecast_target_probability, 1),
  probability_at_issue_pct = round(100 * target_probability, 1),
  change_pp = round(100 * (target_probability -
                             at_forecast_target_probability), 1)
)])
