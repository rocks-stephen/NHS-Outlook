source("R/utils.R")
source("R/national_forecast.R")
source("R/release_outputs.R")

config <- read_key_value_config("config/national_model.csv")
national <- data.table::fread("data-interim/national_month_stage1.csv", encoding = "UTF-8")
national[, calendar_month := data.table::as.IDate(calendar_month)]
validate_national_model_data(national)

rolling_result <- rolling_national_predictions(national, config)
rolling <- rolling_result$predictions
scores <- score_national_predictions(rolling, config)
all_forecasts <- make_final_national_forecasts(national, config)

reference <- all_forecasts[model == config$reference_model]
if (!nrow(reference) || anyNA(reference$predicted_performance)) {
  stop("The configured reference model did not produce a complete final forecast.")
}
reference <- add_empirical_intervals(reference, rolling, config)
deviations <- national_deviation_monitor(rolling, config)

forecast_end <- parse_date_setting(config, "forecast_end_month")
target <- parse_numeric_setting(config, "target_performance", 0, 1)
fy_start <- data.table::as.IDate(sprintf("%d-04-01", as.integer(format(forecast_end, "%Y")) - 1L))
summary <- data.table::data.table(
  metric = c(
    "latest_actual_performance",
    "reference_forecast_fy_2028_29_mean",
    "reference_forecast_march_2029",
    "march_2029_gap_to_target_pp"
  ),
  value = c(
    national$ae4h_performance[nrow(national)],
    reference[forecast_month >= fy_start & forecast_month <= forecast_end,
      mean(predicted_performance)],
    reference[forecast_month == forecast_end, predicted_performance],
    100 * (reference[forecast_month == forecast_end, predicted_performance] - target)
  ),
  unit = c("proportion", "proportion", "proportion", "percentage_points")
)

next_release <- make_national_next_release_output(
  national,
  reference,
  all_forecasts,
  rolling,
  scores$by_horizon,
  config
)
next_release_models <- make_national_next_release_model_table(
  all_forecasts, scores$by_horizon, config
)
selection_start <- parse_date_setting(config, "selection_target_start")
reference_one_step <- rolling[
  model == config$reference_model & horizon_months == 1L &
    target_month >= selection_start
]
reference_one_step_score <- scores$by_horizon[
  model == config$reference_model & horizon_months == 1L &
    evaluation_window == "primary_selection"
]
naive_one_step_score <- scores$by_horizon[
  model == "seasonal_naive" & horizon_months == 1L &
    evaluation_window == "primary_selection"
]
forecast_method <- data.table::data.table(
  metric_id = "ae4h_all",
  display_name = "A&E four-hour performance",
  forecast_method_id = "ae4h_fixed_three_component_ensemble_v1",
  forecast_method_label = paste(
    "Equal-weight ensemble of seasonal persistence, damped annual drift",
    "and a shared-season current-era trend"
  ),
  model_selection_rule = paste(
    "Fixed common ensemble; not selected as the best-fitting model",
    "separately for each release"
  ),
  modelling_scale = paste(
    "Seasonal persistence and drift use the bounded proportion on a logit scale;",
    "the trend component is a quasi-binomial count model"
  ),
  seasonal_persistence_definition =
    "Uses the latest observed value for the same calendar month.",
  annual_drift_definition = paste0(
    "Adds the mean year-on-year logit change from the latest ",
    parse_integer_setting(config, "drift_comparison_months", 1L),
    " comparisons; each additional forecast year is damped by ",
    format(parse_numeric_setting(config, "drift_damping", 0, 1), trim = TRUE),
    "."
  ),
  recent_trend_definition = paste(
    "Fits a quasi-binomial model to counts from the pre-CRS and current eras,",
    "with shared calendar-month effects and separate era-specific trends."
  ),
  ensemble_rule = "Arithmetic mean of all 3 components; all 3 are required.",
  minimum_ensemble_components = 3L,
  components_available_next_release = 3L,
  component_names_next_release = paste(
    c(
      "seasonal_naive", "seasonal_drift_damped",
      "shared_season_current_trend"
    ),
    collapse = ";"
  ),
  minimum_training_months = parse_integer_setting(
    config, "minimum_training_months", 24L
  ),
  final_fit_consecutive_months = nrow(national),
  data_first_month = min(national$calendar_month),
  data_through_month = max(national$calendar_month),
  forecast_month = next_release$forecast_month[1L],
  configured_backtest_months = NA_integer_,
  backtest_rule = paste0(
    "Expanding rolling origins; one-step targets from ", selection_start, "."
  ),
  backtest_predictions_scored = nrow(reference_one_step),
  backtest_first_target_month = if (nrow(reference_one_step)) {
    min(reference_one_step$target_month)
  } else {
    data.table::as.IDate(NA_character_)
  },
  backtest_last_target_month = if (nrow(reference_one_step)) {
    max(reference_one_step$target_month)
  } else {
    data.table::as.IDate(NA_character_)
  },
  reference_mae_native = if (nrow(reference_one_step_score)) {
    reference_one_step_score$mae_pp[1L] / 100
  } else {
    NA_real_
  },
  reference_rmse_native = if (nrow(reference_one_step_score)) {
    reference_one_step_score$rmse_pp[1L] / 100
  } else {
    NA_real_
  },
  reference_bias_native = if (nrow(reference_one_step_score)) {
    reference_one_step_score$bias_pp[1L] / 100
  } else {
    NA_real_
  },
  seasonal_naive_rmse_native = if (nrow(naive_one_step_score)) {
    naive_one_step_score$rmse_pp[1L] / 100
  } else {
    NA_real_
  },
  interval_method = next_release$interval_method[1L],
  interval_calibration_n = next_release$interval_calibration_n[1L],
  interval_definition = paste(
    "Empirical current-era reference-model errors form horizon-specific or",
    "pooled predictive ranges."
  ),
  point_forecast = next_release$predicted_performance[1L],
  lower_80 = next_release$lower_80[1L],
  upper_80 = next_release$upper_80[1L]
)

dir.create("output/national", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(rolling, "output/national/rolling_predictions.csv")
data.table::fwrite(rolling_result$failures, "output/national/model_failures.csv")
data.table::fwrite(scores$by_horizon, "output/national/model_comparison_by_horizon.csv")
data.table::fwrite(scores$pooled, "output/national/model_comparison_pooled.csv")
data.table::fwrite(all_forecasts, "output/national/final_forecasts_all_models.csv")
data.table::fwrite(reference, "output/national/reference_forecast.csv")
data.table::fwrite(deviations, "output/national/deviation_monitor.csv")
data.table::fwrite(summary, "output/national/forecast_summary.csv")
data.table::fwrite(next_release, "output/national/next_release_forecast.csv")
data.table::fwrite(
  next_release_models, "output/national/next_release_model_comparison.csv"
)
data.table::fwrite(forecast_method, "output/national/forecast_method.csv")

release_archive <- write_release_archive(
  "output/national/release_forecast_archive.csv",
  next_release,
  c("forecast_version", "data_through_month", "forecast_month", "model")
)
fresh_release_scores <- score_national_release_archive(
  release_archive[forecast_version == config$release_forecast_version], national
)
release_scorecard <- write_first_release_scorecard(
  "output/national/release_forecast_scorecard.csv",
  fresh_release_scores,
  c("forecast_version", "data_through_month", "forecast_month", "model")
)

message(
  "National modelling complete. Reference FY 2028/29 mean: ",
  sprintf("%.1f%%", 100 * summary[metric == "reference_forecast_fy_2028_29_mean", value]),
  ". Next release forecast for ", format(next_release$forecast_month, "%B %Y"),
  ": ", sprintf("%.1f%%", 100 * next_release$predicted_performance),
  ". Review model comparisons before publication."
)
