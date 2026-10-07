source("R/utils.R")
source("R/national_forecast.R")
source("R/performance_outlook.R")

metric_config <- data.table::fread(
  "config/performance_metrics.csv", encoding = "UTF-8"
)
metric_config[, active := parse_logical_strict(active)]
model_config <- data.table::fread("config/core_model.csv", encoding = "UTF-8")
configured_metrics <- metric_config[
  active == TRUE & adapter == "core_metric", metric_id
]
if (!file.exists("output/core/model_status.csv")) {
  stop("Core metric validation requires output/core/model_status.csv.")
}
model_status <- data.table::fread(
  "output/core/model_status.csv", encoding = "UTF-8"
)
assert_columns(model_status, c(
  "metric_id", "model_status", "status_reason",
  "minimum_viable_residuals", "national_reference_residuals",
  "provider_reference_residuals"
), "core model status")
metrics <- model_status[
  model_status %in% c("included", "included_national_only"), metric_id
]
checks <- list()
check_index <- 0L
add_check <- function(metric_id, check, passed, detail) {
  check_index <<- check_index + 1L
  checks[[check_index]] <<- data.table::data.table(
    metric_id = metric_id,
    check = check,
    passed = isTRUE(passed),
    detail = as.character(detail)
  )
}

add_check(
  "ALL", "one_status_per_configured_metric",
  !anyDuplicated(model_status$metric_id) &&
    setequal(model_status$metric_id, configured_metrics),
  paste(model_status$metric_id, collapse = ",")
)
add_check(
  "ALL", "recognised_model_status",
  all(model_status$model_status %in% c(
    "included", "included_national_only", "excluded_no_data",
    "excluded_insufficient_history"
  )),
  paste(unique(model_status$model_status), collapse = ",")
)
history_path <- "output/qa/core_national_history.csv"
history <- if (file.exists(history_path)) {
  data.table::fread(history_path, encoding = "UTF-8")
} else {
  data.table::data.table()
}
history_ok <- nrow(history) == length(configured_metrics) &&
  all(c(
    "metric_id", "latest_consecutive_months", "required_consecutive_months",
    "supports_full_backtest"
  ) %in% names(history)) &&
  setequal(history$metric_id, configured_metrics) &&
  all(parse_logical_strict(
    history[metric_id %in% metrics, supports_full_backtest]
  ) == TRUE) &&
  all(history[metric_id %in% metrics]$latest_consecutive_months >=
        history[metric_id %in% metrics]$required_consecutive_months)
add_check(
  "ALL", "headline_national_history_supports_full_backtests", history_ok,
  if (nrow(history)) {
    paste(
      history$metric_id,
      paste0(history$latest_consecutive_months, "/",
             history$required_consecutive_months),
      collapse = ";"
    )
  } else {
    paste("missing", history_path)
  }
)
excluded <- model_status[
  model_status %in% c("excluded_no_data", "excluded_insufficient_history")
]
add_check(
  "ALL", "excluded_metrics_are_explained",
  !nrow(excluded) || all(
    !is.na(excluded$status_reason) & nzchar(excluded$status_reason) &
      (
        excluded$national_reference_residuals < excluded$minimum_viable_residuals |
          excluded$provider_reference_residuals < excluded$minimum_viable_residuals |
          grepl("No current", excluded$status_reason, fixed = TRUE)
      )
  ),
  paste(excluded$metric_id, collapse = ",")
)

for (metric in metrics) {
  directory <- file.path("output/core", metric)
  status_row <- model_status[metric_id == metric]
  required_names <- c(
    "national_next_release_forecast.csv",
    "national_reference_projection.csv",
    "provider_latest_watchlist.csv",
    "national_release_forecast_archive.csv",
    "overview_row.csv",
    "forecast_method.csv",
    "national_ensemble_weights.csv",
    "national_forecast_reversal_diagnostic.csv"
  )
  if (status_row$model_status[1L] == "included") {
    required_names <- c(
      required_names,
      "provider_next_release_forecast.csv",
      "provider_release_forecast_archive.csv",
      "provider_ensemble_weights.csv"
    )
  }
  required <- file.path(directory, required_names)
  missing <- required[!file.exists(required)]
  add_check(metric, "required_outputs", !length(missing), paste(missing, collapse = ";"))
  if (length(missing)) next
  national <- data.table::fread(required[1L], encoding = "UTF-8")
  national[, `:=`(
    data_through_month = data.table::as.IDate(data_through_month),
    forecast_month = data.table::as.IDate(forecast_month)
  )]
  provider_path <- file.path(directory, "provider_next_release_forecast.csv")
  provider <- if (
    status_row$model_status[1L] == "included" && file.exists(provider_path)
  ) data.table::fread(provider_path, encoding = "UTF-8") else data.table::data.table()
  if (nrow(provider)) provider[, `:=`(
    data_through_month = data.table::as.IDate(data_through_month),
    forecast_month = data.table::as.IDate(forecast_month)
  )]
  watchlist <- data.table::fread(
    file.path(directory, "provider_latest_watchlist.csv"), encoding = "UTF-8"
  )
  config_row <- model_config[metric_id == metric]
  expected_backtests <- as.integer(config_row$backtest_months[1L])
  method_path <- file.path(directory, "forecast_method.csv")
  method <- data.table::fread(method_path, encoding = "UTF-8")
  method_required <- c(
    "metric_id", "forecast_method_id", "minimum_ensemble_components",
    "components_available_next_release", "final_fit_consecutive_months",
    "configured_backtest_months", "backtest_predictions_scored",
    "interval_method", "interval_calibration_n", "ensemble_weight_summary",
    "ensemble_weighting_target_months", "ensemble_weighting_method"
  )
  method_schema_ok <- all(method_required %in% names(method))
  add_check(
    metric, "forecast_method_record_complete",
    nrow(method) == 1L && method_schema_ok &&
      method$forecast_method_id[1L] ==
        "backtest_weighted_current_level_ensemble_v2",
    paste("rows", nrow(method), "schema", method_schema_ok)
  )
  if (nrow(method) == 1L && method_schema_ok) {
    add_check(
      metric, "forecast_method_matches_run",
      method$final_fit_consecutive_months[1L] == national$training_months_n[1L] &&
        method$configured_backtest_months[1L] == expected_backtests &&
        method$backtest_predictions_scored[1L] == expected_backtests &&
        method$components_available_next_release[1L] >=
          method$minimum_ensemble_components[1L] &&
        method$interval_method[1L] == national$interval_method[1L] &&
        method$interval_calibration_n[1L] == national$interval_calibration_n[1L],
      paste(
        "fit", method$final_fit_consecutive_months[1L],
        "backtests", method$backtest_predictions_scored[1L],
        "components", method$components_available_next_release[1L],
        "interval n", method$interval_calibration_n[1L]
      )
    )
  }
  national_weights <- data.table::fread(
    file.path(directory, "national_ensemble_weights.csv"), encoding = "UTF-8"
  )
  weight_schema_ok <- all(c(
    "model", "ensemble_weight", "component_rmse",
    "weighting_target_months", "weighting_method"
  ) %in% names(national_weights))
  add_check(
    metric, "national_ensemble_weights_valid",
    weight_schema_ok && nrow(national_weights) == 3L &&
      setequal(national_weights$model, c(
        "recent_level_seasonal", "seasonal_drift_damped",
        "recent_trend_seasonal"
      )) && all(is.finite(national_weights$ensemble_weight)) &&
      all(national_weights$ensemble_weight > 0) &&
      abs(sum(national_weights$ensemble_weight) - 1) < 1e-8,
    if (weight_schema_ok) paste0(
      paste(
        national_weights$model,
        round(national_weights$ensemble_weight, 4L),
        collapse = ";"
      ), "; sum=", round(sum(national_weights$ensemble_weight), 8L)
    ) else "weight schema incomplete"
  )
  reversal <- data.table::fread(
    file.path(directory, "national_forecast_reversal_diagnostic.csv"),
    encoding = "UTF-8"
  )
  reversal_schema_ok <- all(c(
    "metric_id", "recent_actual_direction", "recent_direction_months",
    "forecast_change_native", "reversal_threshold_native",
    "forecast_reversal_flag", "forecast_reversal_reason"
  ) %in% names(reversal))
  add_check(
    metric, "forecast_reversal_diagnostic_complete",
    nrow(reversal) == 1L && reversal_schema_ok &&
      !is.na(reversal$forecast_reversal_flag[1L]) &&
      nzchar(reversal$forecast_reversal_reason[1L]),
    if (nrow(reversal) && reversal_schema_ok) {
      paste(
        reversal$recent_actual_direction[1L],
        "flag", reversal$forecast_reversal_flag[1L]
      )
    } else {
      "missing or incomplete diagnostic"
    }
  )
  add_check(
    metric, "national_full_backtest_completed",
    nrow(status_row) == 1L &&
      status_row$national_reference_residuals[1L] == expected_backtests,
    paste(
      "reference residuals", status_row$national_reference_residuals[1L],
      "expected", expected_backtests
    )
  )
  threshold <- as.numeric(config_row$signal_materiality_native[1L])
  share <- as.numeric(config_row$signal_direction_share[1L])
  empirical_interval_n <- as.integer(
    config_row$minimum_interval_residuals[1L]
  )
  parametric_interval_n <- as.integer(
    config_row$minimum_parametric_interval_residuals[1L]
  )
  flagged <- watchlist[signal %in% c("sustained_favourable", "sustained_adverse")]
  add_check(metric, "one_national_next_forecast", nrow(national) == 1L,
            paste("rows", nrow(national)))
  add_check(
    metric, "national_targets_next_month",
    nrow(national) == 1L &&
      national$forecast_month == next_calendar_month(national$data_through_month),
    paste(national$data_through_month, national$forecast_month)
  )
  add_check(
    metric, "national_interval_order",
    nrow(national) == 1L && national$lower_95 <= national$lower_80 &&
      national$lower_80 <= national$predicted_value &&
      national$predicted_value <= national$upper_80 &&
      national$upper_80 <= national$upper_95,
    "lower95 <= lower80 <= point <= upper80 <= upper95"
  )
  add_check(
    metric, "national_interval_calibration",
    nrow(national) == 1L &&
      national$interval_calibration_n >= parametric_interval_n &&
      (
        national$interval_calibration_n >= empirical_interval_n ||
          grepl("student_t_predictive_small_sample", national$interval_method)
      ),
    paste(
      "n", national$interval_calibration_n,
      "method", national$interval_method
    )
  )
  if (status_row$model_status[1L] == "included") {
    add_check(
      metric, "provider_unique_next_forecast",
      !anyDuplicated(provider[, .(entity_id, forecast_month)]),
      paste("rows", nrow(provider))
    )
    add_check(
      metric, "provider_interval_order",
      nrow(provider) > 0L && all(
        provider$lower_95 <= provider$lower_80 &
          provider$lower_80 <= provider$predicted_value &
          provider$predicted_value <= provider$upper_80 &
          provider$upper_80 <= provider$upper_95
      ),
      "lower95 <= lower80 <= point <= upper80 <= upper95"
    )
    add_check(
      metric, "provider_targets_common_next_month",
      nrow(provider) > 0L && data.table::uniqueN(provider$data_through_month) == 1L &&
        data.table::uniqueN(provider$forecast_month) == 1L &&
        provider$forecast_month[1L] == next_calendar_month(provider$data_through_month[1L]),
      paste(unique(provider$forecast_month), collapse = ",")
    )
    add_check(
      metric, "flagged_rows_meet_rule",
      !nrow(flagged) || all(
        abs(flagged$favourable_gap_native) >= threshold &
          flagged$direction_share >= share & flagged$latest_error_same_direction
      ),
      paste("flagged", nrow(flagged))
    )
  } else {
    add_check(
      metric, "provider_watch_withheld_and_labelled",
      !nrow(flagged) && nzchar(status_row$status_reason[1L]),
      status_row$status_reason[1L]
    )
  }
}

method_register_path <- "output/core/forecast_method_register.csv"
method_register <- if (file.exists(method_register_path)) {
  data.table::fread(method_register_path, encoding = "UTF-8")
} else {
  data.table::data.table()
}
add_check(
  "ALL", "forecast_method_register_covers_included_metrics",
  nrow(method_register) == length(metrics) &&
    "metric_id" %in% names(method_register) &&
    setequal(method_register$metric_id, metrics),
  paste("rows", nrow(method_register), "metrics", paste(metrics, collapse = ","))
)

overview <- data.table::fread(
  "output/core/overview_metric_rows.csv", encoding = "UTF-8"
)
overview_ok <- if (!length(metrics)) {
  nrow(overview) == 0L
} else {
  for (column in c("latest_month", "forecast_month", "target_month")) {
    overview[, (column) := data.table::as.IDate(get(column))]
  }
  tryCatch({
    validate_performance_outlook_rows(overview)
    setequal(overview$metric_id, metrics)
  }, error = function(e) {
    message(conditionMessage(e))
    FALSE
  })
}
add_check("ALL", "overview_rows_valid", overview_ok, paste("rows", nrow(overview)))

checks <- data.table::rbindlist(checks, use.names = TRUE, fill = TRUE)
dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(checks, "output/qa/core_metric_output_checks.csv")
if (any(!checks$passed)) {
  stop(
    "Core metric validation failed: ",
    paste(checks[passed == FALSE, paste(metric_id, check, sep = "/")], collapse = ", "),
    ". Inspect output/qa/core_metric_output_checks.csv."
  )
}
message("Core metric validation passed: ", nrow(checks), " checks.")
