next_calendar_month <- function(x) {
  data.table::as.IDate(seq(as.Date(x), by = "month", length.out = 2L)[2L])
}

previous_calendar_year_month <- function(x) {
  data.table::as.IDate(seq(as.Date(x), by = "-1 year", length.out = 2L)[2L])
}

normalise_release_archive_dates <- function(x) {
  out <- data.table::copy(x)
  known_date_columns <- c(
    "calendar_month", "data_through_month", "forecast_month",
    "origin_month", "target_month", "fixed_origin_month"
  )
  class_date_columns <- names(out)[vapply(
    out, function(column) inherits(column, c("Date", "IDate")), logical(1)
  )]
  date_columns <- union(
    intersect(known_date_columns, names(out)),
    class_date_columns
  )
  for (column in date_columns) {
    out[, (column) := as.character(get(column))]
  }
  out[]
}

drop_invalid_archive_keys <- function(x, key_columns, path) {
  if (!nrow(x)) return(x)
  invalid <- rep(FALSE, nrow(x))
  for (column in key_columns) {
    value <- as.character(x[[column]])
    invalid <- invalid | is.na(value) | !nzchar(trimws(value))
  }
  if (any(invalid)) {
    message(
      "Removed ", sum(invalid), " invalid archive row(s) with missing keys from ",
      path, "."
    )
    x <- x[!invalid]
  }
  x[]
}

write_release_archive <- function(path, new_rows, key_columns) {
  assert_columns(new_rows, key_columns)
  incoming <- normalise_release_archive_dates(new_rows)
  if (!"forecast_created_at_utc" %in% names(incoming)) {
    incoming[, forecast_created_at_utc := format(
      Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
    )]
  }
  existing <- if (file.exists(path)) {
    data.table::fread(path, encoding = "UTF-8")
  } else {
    data.table::data.table()
  }
  existing <- normalise_release_archive_dates(existing)
  existing <- drop_invalid_archive_keys(existing, key_columns, path)
  combined <- data.table::rbindlist(
    list(existing, incoming), use.names = TRUE, fill = TRUE
  )
  combined <- unique(combined, by = key_columns)
  data.table::setorderv(combined, key_columns)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(combined, path)
  combined[]
}

write_first_release_scorecard <- function(path, fresh_rows, key_columns) {
  assert_columns(fresh_rows, c(key_columns, "forecast_status"))
  incoming <- normalise_release_archive_dates(fresh_rows)
  now_utc <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  if (!"first_actual_recorded_at_utc" %in% names(incoming)) {
    incoming[, first_actual_recorded_at_utc := data.table::fifelse(
      forecast_status == "scored", now_utc, NA_character_
    )]
  }
  existing <- if (file.exists(path)) {
    data.table::fread(path, encoding = "UTF-8")
  } else {
    data.table::data.table()
  }
  existing <- normalise_release_archive_dates(existing)
  existing <- drop_invalid_archive_keys(existing, key_columns, path)
  existing[, scorecard_source___ := "existing"]
  incoming[, scorecard_source___ := "fresh"]
  combined <- data.table::rbindlist(
    list(existing, incoming), use.names = TRUE, fill = TRUE
  )
  combined[, scorecard_priority___ := data.table::fcase(
    scorecard_source___ == "existing" & forecast_status == "scored", 1L,
    scorecard_source___ == "fresh" & forecast_status == "scored", 2L,
    scorecard_source___ == "fresh", 3L,
    default = 4L
  )]
  data.table::setorderv(
    combined, c(key_columns, "scorecard_priority___"),
    c(rep(1L, length(key_columns)), 1L)
  )
  combined <- unique(combined, by = key_columns)
  combined[, c("scorecard_source___", "scorecard_priority___") := NULL]
  data.table::setorderv(combined, key_columns)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(combined, path)
  combined[]
}

make_national_next_release_output <- function(
    national, reference_forecast, all_forecasts, rolling, comparison_by_horizon, config) {
  next_row <- reference_forecast[horizon_months == 1L]
  if (nrow(next_row) != 1L) stop("Expected one national one-month-ahead forecast row.")
  target_month <- next_row$forecast_month
  latest <- national[calendar_month == max(calendar_month)]
  prior_year <- national[calendar_month == previous_calendar_year_month(target_month)]
  candidate <- all_forecasts[
    horizon_months == 1L & !is.na(predicted_performance)
  ]
  current_start <- parse_date_setting(config, "current_era_start")
  residuals <- rolling[
    model == config$reference_model & horizon_months == 1L &
      target_month >= current_start
  ]
  primary_accuracy <- comparison_by_horizon[
    evaluation_window == "primary_selection" &
      model == config$reference_model & horizon_months == 1L
  ]
  if (!nrow(primary_accuracy)) {
    primary_accuracy <- data.table::data.table(
      n_predictions = NA_integer_, mae_pp = NA_real_, rmse_pp = NA_real_, bias_pp = NA_real_
    )
  }
  version <- if (!is.null(config$release_forecast_version)) {
    config$release_forecast_version
  } else {
    "national_all_ae4h_v1"
  }
  data.table::data.table(
    forecast_version = version,
    data_through_month = latest$calendar_month,
    forecast_month = target_month,
    model = config$reference_model,
    predicted_performance = next_row$predicted_performance,
    lower_80 = next_row$lower_80,
    upper_80 = next_row$upper_80,
    lower_95 = next_row$lower_95,
    upper_95 = next_row$upper_95,
    latest_actual_performance = latest$ae4h_performance,
    forecast_change_from_latest_pp = 100 * (
      next_row$predicted_performance - latest$ae4h_performance
    ),
    same_month_last_year_performance = if (nrow(prior_year)) {
      prior_year$ae4h_performance
    } else {
      NA_real_
    },
    forecast_year_on_year_change_pp = if (nrow(prior_year)) {
      100 * (next_row$predicted_performance - prior_year$ae4h_performance)
    } else {
      NA_real_
    },
    candidate_model_min = min(candidate$predicted_performance),
    candidate_model_max = max(candidate$predicted_performance),
    candidate_model_spread_pp = 100 * diff(range(candidate$predicted_performance)),
    current_era_one_step_n = nrow(residuals),
    current_era_one_step_mae_pp = mean(abs(residuals$residual_pp)),
    current_era_one_step_rmse_pp = sqrt(mean(residuals$residual_pp^2)),
    current_era_one_step_bias_pp = mean(residuals$residual_pp),
    primary_one_step_n = primary_accuracy$n_predictions[1L],
    primary_one_step_mae_pp = primary_accuracy$mae_pp[1L],
    primary_one_step_rmse_pp = primary_accuracy$rmse_pp[1L],
    primary_one_step_bias_pp = primary_accuracy$bias_pp[1L],
    interval_calibration_n = next_row$interval_calibration_n,
    interval_method = next_row$interval_method,
    source_file = latest$source_file,
    source_sha256 = latest$source_sha256
  )
}

make_national_next_release_model_table <- function(
    all_forecasts, comparison_by_horizon, config) {
  target <- min(all_forecasts$forecast_month)
  forecast <- all_forecasts[forecast_month == target, .(
    model,
    forecast_month,
    predicted_performance
  )]
  accuracy <- comparison_by_horizon[
    evaluation_window == "primary_selection" & horizon_months == 1L,
    .(model, primary_one_step_n = n_predictions, primary_one_step_mae_pp = mae_pp,
      primary_one_step_rmse_pp = rmse_pp, primary_one_step_bias_pp = bias_pp)
  ]
  out <- merge(forecast, accuracy, by = "model", all.x = TRUE)
  out[, is_reference := model == config$reference_model]
  data.table::setorder(out, primary_one_step_rmse_pp, model)
  out[]
}

score_national_release_archive <- function(archive, national) {
  if (!nrow(archive)) return(data.table::data.table())
  x <- data.table::copy(archive)
  x[, forecast_month := data.table::as.IDate(forecast_month)]
  actual <- national[, .(
    forecast_month = calendar_month,
    actual_performance = ae4h_performance,
    actual_source_file = source_file,
    actual_source_sha256 = source_sha256
  )]
  out <- merge(x, actual, by = "forecast_month", all.x = TRUE)
  out[, `:=`(
    residual_pp = 100 * (actual_performance - predicted_performance),
    absolute_error_pp = abs(100 * (actual_performance - predicted_performance)),
    forecast_status = data.table::fifelse(
      is.na(actual_performance), "awaiting_release", "scored"
    )
  )]
  data.table::setorder(out, forecast_month)
  out[]
}
