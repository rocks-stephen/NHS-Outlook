source("R/utils.R")
source("R/national_forecast.R")
source("R/release_outputs.R")
source("R/provider_forecast.R")

provider_code <- paste(readLines("R/provider_forecast.R", warn = FALSE), collapse = "\n")
expected_selector <- "submitted <- out[out$complete_submission %in% TRUE]"
if (!grepl(expected_selector, provider_code, fixed = TRUE)) {
  stop(
    "Outdated R/provider_forecast.R detected. Its submitted-row selector must be: ",
    expected_selector
  )
}

config <- read_key_value_config("config/provider_model.csv")
mapping <- data.table::fread("config/trust_mapping.csv", encoding = "UTF-8")
use_mapped_panel <- any(mapping$review_status == "approved")
input_path <- if (use_mapped_panel) {
  "data-interim/provider_month_ae4h_mapped.csv"
} else {
  "data-interim/provider_month_ae4h_unharmonised.csv"
}
if (!file.exists(input_path)) {
  stop("Provider modelling input does not exist: ", input_path,
       ". Run scripts/04_import_provider.R first.")
}

provider_source <- data.table::fread(input_path, encoding = "UTF-8")
provider_source[, calendar_month := data.table::as.IDate(calendar_month)]
panel <- prepare_provider_model_panel(provider_source)

rolling <- rolling_provider_predictions(panel, config)
scores <- score_provider_predictions(rolling, config)
all_forecasts <- make_provider_final_forecasts(panel, config)

reference <- all_forecasts[model == config$reference_model]
if (!nrow(reference)) stop("The configured provider reference model produced no forecasts.")
next_release <- reference[horizon_months == 1L & !is.na(predicted_performance)]
next_release <- add_provider_one_step_intervals(next_release, rolling, config)
next_release <- add_provider_next_release_context(next_release, panel)

interval_columns <- c(
  "analysis_trust_id", "forecast_month", "lower_80", "upper_80",
  "lower_95", "upper_95", "interval_calibration_n", "interval_method"
)
reference <- merge(
  reference,
  next_release[, ..interval_columns],
  by = c("analysis_trust_id", "forecast_month"),
  all.x = TRUE
)
reference[, projection_status := data.table::fifelse(
  horizon_months == 1L,
  "release_ahead_forecast_with_empirical_interval",
  "exploratory_long_range_point_projection"
)]

dir.create("output/provider", recursive = TRUE, showWarnings = FALSE)
archive_rows <- make_provider_release_archive_rows(next_release, config)
release_archive <- write_release_archive(
  "output/provider/release_forecast_archive.csv",
  archive_rows,
  c(
    "forecast_version", "data_through_month", "forecast_month",
    "analysis_trust_id", "model"
  )
)
fresh_release_scores <- score_provider_release_archive(
  release_archive[forecast_version == config$release_forecast_version], panel
)
release_scorecard <- write_first_release_scorecard(
  "output/provider/release_forecast_scorecard.csv",
  fresh_release_scores,
  c(
    "forecast_version", "data_through_month", "forecast_month",
    "analysis_trust_id", "model"
  )
)

surprises <- provider_release_surprise_history(rolling, release_scorecard, config)
deviations <- provider_deviation_monitor(surprises, config)
fixed_origin <- provider_fixed_origin_outlook(panel, config)
watchlist <- make_provider_watchlist(
  panel, next_release, deviations, fixed_origin$summary, config
)
review_list <- watchlist[signal %in% c(
  "sustained_above_trajectory", "sustained_below_trajectory"
)]

latest_month <- max(panel$calendar_month)
eligibility <- panel[, .(
  analysis_trust_name = latest_nonmissing_character(analysis_trust_name),
  first_submitted_month = if (any(complete_submission)) {
    min(calendar_month[complete_submission])
  } else {
    data.table::as.IDate(NA)
  },
  last_submitted_month = if (any(complete_submission)) {
    max(calendar_month[complete_submission])
  } else {
    data.table::as.IDate(NA)
  },
  submitted_months_n = sum(complete_submission),
  missing_or_incomplete_months_n = sum(!complete_submission),
  submitted_in_latest_month = any(calendar_month == latest_month & complete_submission),
  identity_status = latest_nonmissing_character(identity_status),
  source_org_codes = latest_nonmissing_character(source_org_codes)
), by = analysis_trust_id]
eligibility[, forecast_eligible :=
  submitted_in_latest_month &
  submitted_months_n >= parse_integer_setting(config, "minimum_training_months", 12L)
]

summary <- data.table::data.table(
  metric = c(
    "latest_provider_month",
    "providers_submitted_latest_month",
    "providers_with_next_release_forecast",
    "providers_sustained_above_trajectory",
    "providers_sustained_below_trajectory",
    "providers_high_review",
    "providers_with_six_genuine_release_vintages"
  ),
  value = c(
    as.character(latest_month),
    as.character(nrow(panel[
      calendar_month == latest_month & complete_submission & !is.na(ae4h_performance)
    ])),
    as.character(nrow(next_release)),
    as.character(sum(watchlist$signal == "sustained_above_trajectory", na.rm = TRUE)),
    as.character(sum(watchlist$signal == "sustained_below_trajectory", na.rm = TRUE)),
    as.character(sum(watchlist$review_priority == "high_review", na.rm = TRUE)),
    as.character(sum(
      watchlist$signal_evidence == "genuine_release_vintages", na.rm = TRUE
    ))
  )
)

data.table::fwrite(panel, "output/provider/provider_model_panel.csv")
data.table::fwrite(eligibility, "output/provider/provider_eligibility.csv")
data.table::fwrite(rolling, "output/provider/rolling_one_step_predictions.csv")
data.table::fwrite(scores$overall, "output/provider/model_comparison.csv")
data.table::fwrite(scores$by_provider, "output/provider/reference_accuracy_by_provider.csv")
data.table::fwrite(all_forecasts, "output/provider/final_forecasts_all_models.csv")
data.table::fwrite(reference, "output/provider/reference_projection.csv")
data.table::fwrite(next_release, "output/provider/next_release_forecast.csv")
data.table::fwrite(surprises, "output/provider/release_surprise_history.csv")
data.table::fwrite(deviations, "output/provider/deviation_history.csv")
data.table::fwrite(
  fixed_origin$detail, "output/provider/latest_six_month_fixed_outlook.csv"
)
data.table::fwrite(watchlist, "output/provider/latest_watchlist.csv")
data.table::fwrite(review_list, "output/provider/provider_review_list.csv")
data.table::fwrite(summary, "output/provider/provider_summary.csv")

signal_archive_rows <- make_provider_signal_archive_rows(watchlist, config)
signal_archive <- write_release_archive(
  "output/provider/provider_signal_archive.csv",
  signal_archive_rows,
  c("signal_version", "data_through_month", "analysis_trust_id")
)

message(
  "Provider modelling complete. Next release forecasts: ", nrow(next_release),
  "; sustained above trajectory: ",
  sum(watchlist$signal == "sustained_above_trajectory", na.rm = TRUE),
  "; sustained below trajectory: ",
  sum(watchlist$signal == "sustained_below_trajectory", na.rm = TRUE),
  ". Review output/provider/latest_watchlist.csv."
)
