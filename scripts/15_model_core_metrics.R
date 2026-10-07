source("R/utils.R")
source("R/national_forecast.R")
source("R/release_outputs.R")
source("R/core_import.R")
source("R/core_forecast.R")

required_files <- c(
  "data-interim/core/national_panel.csv",
  "data-interim/core/provider_panel.csv",
  "config/performance_metrics.csv",
  "config/core_model.csv",
  "config/core_targets.csv"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Cannot model core metrics; missing: ", paste(missing_files, collapse = ", "))
}

national_panel <- data.table::fread(
  "data-interim/core/national_panel.csv", encoding = "UTF-8"
)
provider_panel <- data.table::fread(
  "data-interim/core/provider_panel.csv", encoding = "UTF-8"
)
national_panel[, calendar_month := data.table::as.IDate(calendar_month)]
provider_panel[, calendar_month := data.table::as.IDate(calendar_month)]
metric_config <- data.table::fread(
  "config/performance_metrics.csv", encoding = "UTF-8"
)
metric_config[, `:=`(
  active = parse_logical_strict(active),
  higher_is_better = parse_logical_strict(higher_is_better),
  provider_signal_enabled = parse_logical_strict(provider_signal_enabled)
)]
model_config <- data.table::fread("config/core_model.csv", encoding = "UTF-8")
targets <- data.table::fread("config/core_targets.csv", encoding = "UTF-8")
targets[, target_month := data.table::as.IDate(target_month)]
core_metrics <- metric_config[active == TRUE & adapter == "core_metric"]
if (!nrow(core_metrics)) stop("No active core metrics are configured.")

overview_rows <- list()
status_rows <- list()
method_rows <- list()
core_model_status_row <- function(metric_row, status, reason,
                                  minimum_viable_residuals,
                                  national_reference_residuals = NA_integer_,
                                  provider_reference_residuals = NA_integer_,
                                  providers_forecast = NA_integer_) {
  data.table::data.table(
    metric_id = metric_row$metric_id[1L],
    display_name = metric_row$display_name[1L],
    model_status = status,
    status_reason = reason,
    minimum_viable_residuals = as.integer(minimum_viable_residuals),
    national_reference_residuals = as.integer(national_reference_residuals),
    provider_reference_residuals = as.integer(provider_reference_residuals),
    providers_forecast = as.integer(providers_forecast)
  )
}
write_optional_core_output <- function(x, path) {
  if (!ncol(x)) {
    # A national-only metric must not leave a stale provider file from an
    # earlier run in which provider modelling happened to be available.
    if (file.exists(path) && !file.remove(path)) {
      stop("Could not remove stale optional output: ", path, ".")
    }
    return(invisible(path))
  }
  data.table::fwrite(x, path)
  invisible(path)
}
for (metric_index in seq_len(nrow(core_metrics))) {
  metric_row <- core_metrics[metric_index]
  metric_id_value <- metric_row$metric_id[1L]
  config_row <- model_config[metric_id == metric_id_value]
  metric_targets <- targets[metric_id == metric_id_value]
  if (nrow(config_row) != 1L || !nrow(metric_targets)) {
    stop("Missing model configuration or targets for ", metric_id_value, ".")
  }
  minimum_viable_residuals <- core_config_integer(
    config_row, "minimum_parametric_interval_residuals", 5L,
    core_config_integer(config_row, "minimum_interval_residuals", 5L)
  )
  national <- national_panel[
    metric_id == metric_id_value & complete_submission == TRUE & is.finite(value)
  ]
  provider <- provider_panel[
    metric_id == metric_id_value &
      calendar_month >= data.table::as.IDate(config_row$current_era_start[1L])
  ]
  if (!nrow(national)) {
    reason <- "No complete national observations were imported."
    status_rows[[metric_index]] <- core_model_status_row(
      metric_row, "excluded_no_data", reason,
      minimum_viable_residuals, 0L, 0L, 0L
    )
    message(metric_id_value, ": excluded from this release — ", reason)
    next
  }
  if (nrow(provider) && max(national$calendar_month) != max(provider$calendar_month)) {
    message(
      metric_id_value, ": provider watch withheld because national and provider ",
      "latest months differ (", max(national$calendar_month), " versus ",
      max(provider$calendar_month), ")."
    )
    provider <- provider[0]
  }
  minimum_training_months <- core_config_integer(
    config_row, "minimum_training_months", 12L
  )
  backtest_months <- core_config_integer(
    config_row, "backtest_months", 6L
  )
  national_contiguous_history <- core_contiguous_tail(national)
  required_national_history <- minimum_training_months + backtest_months
  if (nrow(national_contiguous_history) < required_national_history) {
    reason <- paste0(
      "Only ", nrow(national_contiguous_history), " consecutive national months; ",
      required_national_history, " are required for the configured training and ",
      "full backtest windows."
    )
    status_rows[[metric_index]] <- core_model_status_row(
      metric_row, "excluded_insufficient_history", reason,
      minimum_viable_residuals, 0L, 0L, 0L
    )
    message(metric_id_value, ": excluded from this release — ", reason)
    next
  }
  national_rolling <- core_rolling_one_step(
    national, metric_row, config_row, allow_empty = TRUE
  )
  national_reference_residuals <- if (nrow(national_rolling)) {
    national_rolling[
      model == "reference_ensemble" & is.finite(error_native), .N
    ]
  } else {
    0L
  }
  if (national_reference_residuals < backtest_months) {
    stop(
      metric_id_value, " has ", nrow(national_contiguous_history),
      " consecutive national months but produced only ",
      national_reference_residuals, " of ", backtest_months,
      " expected reference-model backtest residuals. This indicates a model ",
      "availability problem, not a short time series."
    )
  }
  if (national_reference_residuals < minimum_viable_residuals) {
    stop(
      metric_id_value, " produced only ", national_reference_residuals,
      " usable national one-step residuals; at least ",
      minimum_viable_residuals, " are required. Inspect the national model rather ",
      "than omitting this headline metric."
    )
  }
  national_scores <- core_score_predictions(national_rolling)
  national_weights <- core_estimate_ensemble_weights(
    national_rolling, config_row
  )
  national_all_forecasts <- core_make_final_forecasts(
    national, metric_row, config_row,
    ensemble_weights = national_weights, allow_empty = TRUE
  )
  if (!nrow(national_all_forecasts)) {
    stop(
      "No current national forecast met the configured requirements for ",
      metric_id_value, ". Inspect the national model rather than omitting this ",
      "headline metric."
    )
  }
  national_reference <- national_all_forecasts[
    model == "reference_ensemble" & is.finite(predicted_value)
  ]
  if (!nrow(national_reference[horizon_months == 1L])) {
    stop(
      "No current national reference forecast was available for the next ",
      "release of ", metric_id_value, ". Inspect the national model rather than ",
      "omitting this headline metric."
    )
  }
  national_reference <- core_add_intervals(
    national_reference, national_rolling, metric_row, config_row
  )
  national_next <- national_reference[horizon_months == 1L]
  if (nrow(national_next) != 1L) {
    stop("Expected one England next-release forecast for ", metric_id_value, ".")
  }
  reversal_diagnostic <- core_forecast_reversal_diagnostic(
    national, national_next, metric_row, config_row
  )
  if (isTRUE(reversal_diagnostic$forecast_reversal_flag[1L])) {
    message(
      metric_id_value, ": forecast reversal review — ",
      reversal_diagnostic$forecast_reversal_reason[1L]
    )
  }
  national_next <- merge(
    national_next, reversal_diagnostic,
    by = c("metric_id", "data_through_month", "forecast_month"),
    all.x = TRUE
  )

  metric_dir <- file.path("output/core", metric_id_value)
  dir.create(metric_dir, recursive = TRUE, showWarnings = FALSE)
  method_record <- core_forecast_method_record(
    metric_row, config_row, national, national_rolling, national_scores,
    national_all_forecasts, national_next, national_weights
  )
  method_rows[[metric_index]] <- method_record
  national_archive <- write_release_archive(
    file.path(metric_dir, "national_release_forecast_archive.csv"),
    core_archive_rows(national_next, metric_row, config_row),
    c(
      "metric_id", "forecast_version", "data_through_month", "forecast_month",
      "entity_id", "model"
    )
  )
  national_scorecard <- write_first_release_scorecard(
    file.path(metric_dir, "national_release_forecast_scorecard.csv"),
    core_score_release_archive(national_archive, national),
    c(
      "metric_id", "forecast_version", "data_through_month", "forecast_month",
      "entity_id", "model"
    )
  )
  provider_rolling <- data.table::data.table()
  provider_scores <- data.table::data.table()
  provider_reference <- data.table::data.table()
  provider_next <- data.table::data.table()
  provider_weights <- data.table::data.table()
  surprises <- data.table::data.table()
  watchlist <- core_empty_provider_watchlist()
  provider_reference_residuals <- 0L
  provider_status_reason <- "Provider trajectory watch is disabled for this measure."
  provider_ready <- isTRUE(metric_row$provider_signal_enabled[1L]) && nrow(provider) > 0L
  if (provider_ready) {
    provider_rolling <- core_rolling_one_step(
      provider, metric_row, config_row, allow_empty = TRUE
    )
    provider_reference_residuals <- if (nrow(provider_rolling)) {
      provider_rolling[
        model == "reference_ensemble" & is.finite(error_native), .N
      ]
    } else {
      0L
    }
    provider_ready <- provider_reference_residuals >= minimum_viable_residuals
    if (!provider_ready) {
      provider_status_reason <- paste0(
        "National forecast included; provider watch withheld because only ",
        provider_reference_residuals, " usable pooled provider residuals were ",
        "available (", minimum_viable_residuals, " required)."
      )
    }
  } else if (isTRUE(metric_row$provider_signal_enabled[1L])) {
    provider_status_reason <- paste0(
      "National forecast included; provider watch withheld because no current-era ",
      "provider observations were available."
    )
  }
  if (provider_ready) {
    provider_scores <- core_score_predictions(provider_rolling)
    provider_weights <- core_estimate_ensemble_weights(
      provider_rolling, config_row
    )
    provider_all_forecasts <- core_make_final_forecasts(
      provider, metric_row, config_row,
      ensemble_weights = provider_weights, allow_empty = TRUE
    )
    provider_reference <- provider_all_forecasts[
      model == "reference_ensemble" & is.finite(predicted_value)
    ]
    provider_ready <- nrow(provider_reference[horizon_months == 1L]) > 0L
    if (!provider_ready) {
      provider_status_reason <- paste0(
        "National forecast included; provider watch withheld because no current ",
        "provider next-release forecasts met the training-history requirement."
      )
    }
  }
  if (provider_ready) {
    provider_reference <- core_add_intervals(
      provider_reference, provider_rolling, metric_row, config_row,
      provider_pool = TRUE
    )
    provider_next <- provider_reference[horizon_months == 1L]
    provider_archive <- write_release_archive(
      file.path(metric_dir, "provider_release_forecast_archive.csv"),
      core_archive_rows(provider_next, metric_row, config_row),
      c(
        "metric_id", "forecast_version", "data_through_month", "forecast_month",
        "entity_id", "model"
      )
    )
    provider_scorecard <- write_first_release_scorecard(
      file.path(metric_dir, "provider_release_forecast_scorecard.csv"),
      core_score_release_archive(provider_archive, provider),
      c(
        "metric_id", "forecast_version", "data_through_month", "forecast_month",
        "entity_id", "model"
      )
    )
    surprises <- core_surprise_history(
      provider_rolling, provider_scorecard, config_row
    )
    signals <- core_provider_signals(
      surprises, provider, metric_row, config_row
    )
    watchlist <- core_provider_watchlist(signals, provider, provider_next)
    provider_status_reason <- "Sufficient national and provider history."
  }

  if (metric_id_value == "ucr_2h") {
    ucr_eligibility <- if (nrow(watchlist)) {
      watchlist[, .(
        metric_id, entity_id, entity_name, data_through_month,
        signal_eligibility_reason, signal_months_n, volume_measure,
        minimum_volume_in_signal_window, minimum_required_volume,
        latest_value, latest_activity_volume_proxy, signal
      )]
    } else {
      data.table::data.table(
        metric_id = character(), entity_id = character(), entity_name = character(),
        data_through_month = data.table::as.IDate(character()),
        signal_eligibility_reason = character(), signal_months_n = integer(),
        volume_measure = character(), minimum_volume_in_signal_window = numeric(),
        minimum_required_volume = numeric(), latest_value = numeric(),
        latest_activity_volume_proxy = numeric(), signal = character()
      )
    }
    dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(
      ucr_eligibility, "output/qa/ucr_provider_model_eligibility.csv"
    )
  }

  overview <- core_overview_row(
    metric_row, national, national_reference, national_next, national_archive,
    national_scorecard, watchlist, metric_targets
  )
  overview_rows[[metric_index]] <- overview

  data.table::fwrite(national_rolling, file.path(metric_dir, "national_rolling_predictions.csv"))
  data.table::fwrite(national_scores, file.path(metric_dir, "national_model_comparison.csv"))
  data.table::fwrite(national_weights, file.path(metric_dir, "national_ensemble_weights.csv"))
  data.table::fwrite(national_all_forecasts, file.path(metric_dir, "national_all_model_forecasts.csv"))
  data.table::fwrite(national_reference, file.path(metric_dir, "national_reference_projection.csv"))
  data.table::fwrite(national_next, file.path(metric_dir, "national_next_release_forecast.csv"))
  write_optional_core_output(
    provider_rolling, file.path(metric_dir, "provider_rolling_predictions.csv")
  )
  write_optional_core_output(
    provider_scores, file.path(metric_dir, "provider_model_comparison.csv")
  )
  write_optional_core_output(
    provider_weights, file.path(metric_dir, "provider_ensemble_weights.csv")
  )
  write_optional_core_output(
    provider_reference, file.path(metric_dir, "provider_reference_projection.csv")
  )
  write_optional_core_output(
    provider_next, file.path(metric_dir, "provider_next_release_forecast.csv")
  )
  write_optional_core_output(
    surprises, file.path(metric_dir, "provider_release_surprise_history.csv")
  )
  data.table::fwrite(watchlist, file.path(metric_dir, "provider_latest_watchlist.csv"))
  data.table::fwrite(
    reversal_diagnostic,
    file.path(metric_dir, "national_forecast_reversal_diagnostic.csv")
  )
  data.table::fwrite(overview, file.path(metric_dir, "overview_row.csv"))
  data.table::fwrite(method_record, file.path(metric_dir, "forecast_method.csv"))
  data.table::fwrite(data.table::data.table(
    metric_id = metric_id_value,
    latest_month = max(national$calendar_month),
    next_release_month = national_next$forecast_month[1L],
    national_latest_value = national_next$latest_actual_value[1L],
    national_next_forecast = national_next$predicted_value[1L],
    providers_forecast = nrow(provider_next),
    providers_favourable = sum(watchlist$signal == "sustained_favourable", na.rm = TRUE),
    providers_adverse = sum(watchlist$signal == "sustained_adverse", na.rm = TRUE)
  ), file.path(metric_dir, "summary.csv"))
  status_rows[[metric_index]] <- core_model_status_row(
    metric_row, if (provider_ready) "included" else "included_national_only",
    provider_status_reason,
    minimum_viable_residuals, national_reference_residuals,
    provider_reference_residuals, nrow(provider_next)
  )
  message(
    metric_id_value, ": forecast for ", national_next$forecast_month[1L],
    "; provider watch ", if (provider_ready) "included" else "withheld",
    "; signals favourable ",
    sum(watchlist$signal == "sustained_favourable", na.rm = TRUE),
    ", adverse ", sum(watchlist$signal == "sustained_adverse", na.rm = TRUE), "."
  )
}

overview_rows <- data.table::rbindlist(overview_rows, use.names = TRUE, fill = TRUE)
if (nrow(overview_rows)) {
  data.table::setorder(overview_rows, display_order)
} else {
  overview_rows <- data.table::data.table(metric_id = character())
}
status_rows <- data.table::rbindlist(status_rows, use.names = TRUE, fill = TRUE)
method_rows <- data.table::rbindlist(method_rows, use.names = TRUE, fill = TRUE)
dir.create("output/core", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(overview_rows, "output/core/overview_metric_rows.csv")
data.table::fwrite(status_rows, "output/core/model_status.csv")
data.table::fwrite(method_rows, "output/core/forecast_method_register.csv")
message(
  "Core metric modelling complete: ",
  sum(status_rows$model_status == "included"), " with provider watch; ",
  sum(status_rows$model_status == "included_national_only"),
  " national-only; ",
  sum(status_rows$model_status == "excluded_no_data"), " excluded because no ",
  "national observations were imported; ",
  sum(status_rows$model_status == "excluded_insufficient_history"),
  " excluded because the imported series was too short."
)
