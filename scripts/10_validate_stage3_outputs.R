source("R/utils.R")
source("R/national_forecast.R")
source("R/release_outputs.R")

record_check <- function(name, passed, detail) {
  data.table::data.table(
    check = name,
    passed = isTRUE(passed),
    detail = as.character(detail)
  )
}

required_files <- c(
  "output/national/next_release_forecast.csv",
  "output/national/release_forecast_archive.csv",
  "output/national/release_forecast_scorecard.csv",
  "output/national/forecast_method.csv",
  "output/provider/next_release_forecast.csv",
  "output/provider/reference_projection.csv",
  "output/provider/latest_watchlist.csv",
  "output/provider/release_forecast_archive.csv",
  "output/provider/release_forecast_scorecard.csv",
  "output/provider/release_surprise_history.csv",
  "output/provider/latest_six_month_fixed_outlook.csv",
  "output/provider/provider_signal_archive.csv"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing stage-three outputs: ", paste(missing_files, collapse = ", "))
}

national <- data.table::fread(
  "output/national/next_release_forecast.csv", encoding = "UTF-8"
)
national[, `:=`(
  data_through_month = data.table::as.IDate(data_through_month),
  forecast_month = data.table::as.IDate(forecast_month)
)]
national_archive <- data.table::fread(
  "output/national/release_forecast_archive.csv", encoding = "UTF-8"
)
national_method <- data.table::fread(
  "output/national/forecast_method.csv", encoding = "UTF-8"
)
provider <- data.table::fread(
  "output/provider/next_release_forecast.csv", encoding = "UTF-8"
)
provider[, `:=`(
  data_through_month = data.table::as.IDate(data_through_month),
  forecast_month = data.table::as.IDate(forecast_month)
)]
projection <- data.table::fread(
  "output/provider/reference_projection.csv", encoding = "UTF-8"
)
watchlist <- data.table::fread(
  "output/provider/latest_watchlist.csv", encoding = "UTF-8"
)
provider_archive <- data.table::fread(
  "output/provider/release_forecast_archive.csv", encoding = "UTF-8"
)
surprises <- data.table::fread(
  "output/provider/release_surprise_history.csv", encoding = "UTF-8"
)
fixed_outlook <- data.table::fread(
  "output/provider/latest_six_month_fixed_outlook.csv", encoding = "UTF-8"
)
signal_archive <- data.table::fread(
  "output/provider/provider_signal_archive.csv", encoding = "UTF-8"
)
provider_config <- read_key_value_config("config/provider_model.csv")
materiality <- parse_numeric_setting(
  provider_config, "persistent_materiality_pp", 0
)
direction_share <- parse_numeric_setting(
  provider_config, "persistent_direction_share", 0.5, 1
)

interval_order <- function(x) {
  all(
    x$lower_95 <= x$lower_80 &
    x$lower_80 <= x$predicted_performance &
    x$predicted_performance <= x$upper_80 &
    x$upper_80 <= x$upper_95
  )
}

archive_keys_complete <- function(x, columns) {
  all(vapply(columns, function(column) {
    value <- as.character(x[[column]])
    all(!is.na(value) & nzchar(trimws(value)))
  }, logical(1)))
}

flagged <- watchlist[signal %in% c(
  "sustained_above_trajectory", "sustained_below_trajectory"
)]
checks <- data.table::rbindlist(list(
  record_check(
    "national_forecast_method_record",
    nrow(national_method) == 1L &&
      national_method$forecast_method_id[1L] ==
        "ae4h_fixed_three_component_ensemble_v1" &&
      national_method$components_available_next_release[1L] == 3L &&
      national_method$interval_method[1L] == national$interval_method[1L] &&
      national_method$interval_calibration_n[1L] ==
        national$interval_calibration_n[1L],
    paste(
      "method", national_method$forecast_method_id[1L],
      "backtests", national_method$backtest_predictions_scored[1L]
    )
  ),
  record_check(
    "national_one_row", nrow(national) == 1L,
    paste("rows:", nrow(national))
  ),
  record_check(
    "national_targets_next_month",
    nrow(national) == 1L &&
      national$forecast_month == next_calendar_month(national$data_through_month),
    paste(national$data_through_month, "to", national$forecast_month)
  ),
  record_check(
    "national_interval_order", interval_order(national),
    "lower 95 <= lower 80 <= point <= upper 80 <= upper 95"
  ),
  record_check(
    "national_archive_unique",
    !anyDuplicated(national_archive[, .(
      forecast_version, data_through_month, forecast_month, model
    )]),
    paste("rows:", nrow(national_archive))
  ),
  record_check(
    "national_archive_keys_complete",
    archive_keys_complete(
      national_archive,
      c("forecast_version", "data_through_month", "forecast_month", "model")
    ),
    "No missing national archive key fields"
  ),
  record_check(
    "provider_unique_next_forecast",
    !anyDuplicated(provider[, .(analysis_trust_id, forecast_month)]),
    paste("rows:", nrow(provider))
  ),
  record_check(
    "provider_targets_one_common_next_month",
    data.table::uniqueN(provider$data_through_month) == 1L &&
      data.table::uniqueN(provider$forecast_month) == 1L &&
      all(provider$forecast_month == next_calendar_month(
        provider$data_through_month[1L]
      )),
    paste(
      "data through:", paste(unique(provider$data_through_month), collapse = ","),
      "target:", paste(unique(provider$forecast_month), collapse = ",")
    )
  ),
  record_check(
    "provider_interval_order", interval_order(provider),
    "lower 95 <= lower 80 <= point <= upper 80 <= upper 95"
  ),
  record_check(
    "provider_projection_bounded",
    all(projection$predicted_performance > 0 & projection$predicted_performance < 1,
        na.rm = TRUE),
    paste("rows:", nrow(projection))
  ),
  record_check(
    "watchlist_unique_provider",
    !anyDuplicated(watchlist$analysis_trust_id),
    paste("rows:", nrow(watchlist))
  ),
  record_check(
    "flagged_rows_meet_rule",
    !nrow(flagged) || all(
      abs(flagged$six_month_gap_to_trajectory_pp) >= materiality &
      flagged$six_month_direction_share >= direction_share &
      flagged$latest_error_same_direction
    ),
    paste("flagged rows:", nrow(flagged))
  ),
  record_check(
    "provider_surprise_unique",
    !anyDuplicated(surprises[, .(analysis_trust_id, target_month)]),
    paste("rows:", nrow(surprises))
  ),
  record_check(
    "provider_surprise_evidence_labelled",
    all(surprises$forecast_evidence %in% c(
      "genuine_release_vintage", "historically_simulated"
    )),
    paste("genuine rows:", sum(
      surprises$forecast_evidence == "genuine_release_vintage", na.rm = TRUE
    ))
  ),
  record_check(
    "fixed_origin_six_month_context",
    !nrow(fixed_outlook) || all(
      fixed_outlook[, .N, by = analysis_trust_id]$N ==
        parse_integer_setting(provider_config, "persistent_window_months", 2L)
    ),
    paste("providers:", data.table::uniqueN(fixed_outlook$analysis_trust_id))
  ),
  record_check(
    "provider_archive_unique",
    !anyDuplicated(provider_archive[, .(
      forecast_version, data_through_month, forecast_month,
      analysis_trust_id, model
    )]),
    paste("rows:", nrow(provider_archive))
  ),
  record_check(
    "provider_archive_keys_complete",
    archive_keys_complete(
      provider_archive,
      c(
        "forecast_version", "data_through_month", "forecast_month",
        "analysis_trust_id", "model"
      )
    ),
    "No missing provider archive key fields"
  ),
  record_check(
    "provider_signal_archive_unique",
    !anyDuplicated(signal_archive[, .(
      signal_version, data_through_month, analysis_trust_id
    )]),
    paste("rows:", nrow(signal_archive))
  ),
  record_check(
    "provider_signal_archive_keys_complete",
    archive_keys_complete(
      signal_archive,
      c("signal_version", "data_through_month", "analysis_trust_id")
    ),
    "No missing provider signal archive key fields"
  )
), use.names = TRUE)

dir.create("output/qa", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(checks, "output/qa/stage3_output_checks.csv")
if (any(!checks$passed)) {
  stop(
    "Stage-three output validation failed: ",
    paste(checks[passed == FALSE, check], collapse = ", "),
    ". Inspect output/qa/stage3_output_checks.csv."
  )
}
message("Stage-three output validation passed: ", nrow(checks), " checks.")
