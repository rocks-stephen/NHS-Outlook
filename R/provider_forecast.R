latest_nonmissing_character <- function(x) {
  z <- as.character(x)
  z <- z[!is.na(z) & nzchar(trimws(z))]
  if (length(z)) utils::tail(z, 1L) else NA_character_
}

collapse_unique_character <- function(x) {
  z <- sort(unique(as.character(x[!is.na(x) & nzchar(trimws(x))])))
  if (length(z)) paste(z, collapse = ";") else NA_character_
}

prepare_provider_model_panel <- function(x) {
  required <- c(
    "calendar_month", "source_org_code", "source_org_name",
    "analysis_trust_id", "analysis_trust_name", "identity_status",
    "ae4h_submission_status", "ae4h_attendances_n", "ae4h_within_4h_n",
    "ae4h_over_4h_n"
  )
  assert_columns(x, required)
  z <- data.table::copy(x)
  z[, calendar_month := data.table::as.IDate(calendar_month)]
  z[, submitted_row :=
    !is.na(ae4h_submission_status) & ae4h_submission_status == "submitted" &
    !is.na(ae4h_attendances_n) & ae4h_attendances_n > 0 &
    !is.na(ae4h_within_4h_n) & !is.na(ae4h_over_4h_n)
  ]
  out <- z[, {
    complete <- all(submitted_row)
    attendances <- if (complete) sum(as.numeric(ae4h_attendances_n)) else NA_real_
    within <- if (complete) sum(as.numeric(ae4h_within_4h_n)) else NA_real_
    over <- if (complete) sum(as.numeric(ae4h_over_4h_n)) else NA_real_
    list(
      analysis_trust_name = latest_nonmissing_character(analysis_trust_name),
      source_org_codes = collapse_unique_character(source_org_code),
      source_org_names = collapse_unique_character(source_org_name),
      identity_status = collapse_unique_character(identity_status),
      source_rows_n = .N,
      submitted_source_rows_n = sum(submitted_row),
      complete_submission = complete,
      ae4h_attendances_n = attendances,
      ae4h_within_4h_n = within,
      ae4h_over_4h_n = over,
      ae4h_performance = if (complete && attendances > 0) within / attendances else NA_real_
    )
  }, by = .(analysis_trust_id, calendar_month)]
  data.table::setorder(out, analysis_trust_id, calendar_month)
  if (anyDuplicated(out[, .(analysis_trust_id, calendar_month)])) {
    stop("Prepared provider panel contains duplicate provider-month keys.")
  }
  submitted <- out[out$complete_submission %in% TRUE]
  if (any(abs(
    submitted$ae4h_attendances_n - submitted$ae4h_within_4h_n -
      submitted$ae4h_over_4h_n
  ) > 1e-8)) {
    stop("Prepared provider counts fail the denominator identity.")
  }
  out[]
}

provider_previous_year_month <- function(x) {
  data.table::as.IDate(seq(as.Date(x), by = "-1 year", length.out = 2L)[2L])
}

provider_predict_seasonal_naive <- function(train, future_dates) {
  vapply(future_dates, function(target) {
    prior <- train[calendar_month == provider_previous_year_month(target)]
    if (nrow(prior) != 1L) NA_real_ else prior$ae4h_performance
  }, numeric(1))
}

provider_predict_seasonal_mean <- function(train, future_dates, years = 3L) {
  vapply(future_dates, function(target) {
    candidates <- train[month_number(calendar_month) == month_number(target)]
    if (!nrow(candidates)) return(NA_real_)
    mean(utils::tail(candidates$ae4h_performance, years))
  }, numeric(1))
}

provider_year_on_year_logit_changes <- function(train) {
  if (nrow(train) < 13L) return(numeric())
  dates <- as.character(train$calendar_month)
  prior_dates <- as.character(vapply(
    train$calendar_month,
    function(x) as.character(provider_previous_year_month(x)),
    character(1)
  ))
  prior_index <- match(prior_dates, dates)
  keep <- !is.na(prior_index)
  if (!any(keep)) return(numeric())
  logit_probability(train$ae4h_performance[keep]) -
    logit_probability(train$ae4h_performance[prior_index[keep]])
}

provider_predict_seasonal_drift <- function(
    train, future_dates, comparison_months, damping) {
  changes <- provider_year_on_year_logit_changes(train)
  if (!length(changes)) return(rep(NA_real_, length(future_dates)))
  annual_drift <- mean(utils::tail(changes, comparison_months))
  vapply(future_dates, function(target) {
    candidates <- train[month_number(calendar_month) == month_number(target)]
    if (!nrow(candidates)) return(NA_real_)
    base <- candidates[nrow(candidates)]
    cycles <- as.integer((month_id(target) - month_id(base$calendar_month)) / 12L)
    if (cycles < 1L) return(NA_real_)
    drift_multiplier <- if (abs(damping - 1) < 1e-12) {
      cycles
    } else {
      (1 - damping^cycles) / (1 - damping)
    }
    stats::plogis(
      logit_probability(base$ae4h_performance) + annual_drift * drift_multiplier
    )
  }, numeric(1))
}

provider_predict_count_glm <- function(train, future_dates, window_months) {
  fit_data <- data.table::copy(utils::tail(train, window_months))
  if (nrow(fit_data) < 24L || data.table::uniqueN(month_number(
    fit_data$calendar_month
  )) < 12L) {
    return(rep(NA_real_, length(future_dates)))
  }
  origin_id <- month_id(max(fit_data$calendar_month))
  fit_data[, `:=`(
    trend = month_id(calendar_month) - origin_id,
    month_factor = factor(month_number(calendar_month), levels = 1:12)
  )]
  fit <- stats::glm(
    cbind(ae4h_within_4h_n, ae4h_over_4h_n) ~ trend + month_factor,
    family = stats::quasibinomial(link = "logit"),
    data = fit_data
  )
  new_data <- data.frame(
    trend = month_id(future_dates) - origin_id,
    month_factor = factor(month_number(future_dates), levels = 1:12)
  )
  bound_probability(stats::predict(fit, newdata = new_data, type = "response"))
}

predict_provider_model_set <- function(train, future_dates, config) {
  comparison_months <- parse_integer_setting(
    config, "drift_comparison_months", 1L
  )
  damping <- parse_numeric_setting(config, "drift_damping", 0, 1)
  glm_window <- parse_integer_setting(config, "glm_window_months", 24L)
  minimum_components <- parse_integer_setting(
    config, "minimum_ensemble_components", 1L, 3L
  )
  safe_prediction <- function(expression) {
    suppressWarnings(tryCatch(
      expression,
      error = function(e) rep(NA_real_, length(future_dates))
    ))
  }
  components <- list(
    seasonal_naive = safe_prediction(
      provider_predict_seasonal_naive(train, future_dates)
    ),
    seasonal_mean_3y = safe_prediction(
      provider_predict_seasonal_mean(train, future_dates, years = 3L)
    ),
    seasonal_drift_damped = safe_prediction(
      provider_predict_seasonal_drift(
        train, future_dates, comparison_months, damping
      )
    ),
    count_glm_trend_recent = safe_prediction(
      provider_predict_count_glm(train, future_dates, glm_window)
    )
  )
  ensemble_components <- cbind(
    components$seasonal_naive,
    components$seasonal_drift_damped,
    components$count_glm_trend_recent
  )
  available <- rowSums(!is.na(ensemble_components))
  ensemble <- rowMeans(ensemble_components, na.rm = TRUE)
  ensemble[available < minimum_components] <- NA_real_
  components$provider_ensemble <- ensemble
  requested <- parse_character_vector_setting(config, "candidate_models")
  unknown <- setdiff(requested, names(components))
  if (length(unknown)) stop("Unknown provider model(s): ", paste(unknown, collapse = ", "))
  data.table::rbindlist(lapply(requested, function(model_id) {
    data.table::data.table(
      model = model_id,
      forecast_month = future_dates,
      horizon_months = seq_along(future_dates),
      predicted_performance = components[[model_id]]
    )
  }))
}

rolling_provider_predictions <- function(panel, config) {
  minimum_training <- parse_integer_setting(config, "minimum_training_months", 12L)
  providers <- unique(panel$analysis_trust_id)
  pieces <- vector("list", length(providers))
  for (provider_index in seq_along(providers)) {
    provider_id <- providers[provider_index]
    provider <- panel[
      analysis_trust_id == provider_id & complete_submission &
        !is.na(ae4h_performance)
    ]
    data.table::setorder(provider, calendar_month)
    provider_predictions <- list()
    prediction_index <- 0L
    if (nrow(provider) <= minimum_training) next
    for (target_index in seq_len(nrow(provider))) {
      target <- provider$calendar_month[target_index]
      origin <- data.table::as.IDate(seq(
        as.Date(target), by = "-1 month", length.out = 2L
      )[2L])
      train <- provider[calendar_month <= origin]
      if (nrow(train) < minimum_training || max(train$calendar_month) != origin) next
      prediction_set <- predict_provider_model_set(train, target, config)
      prediction_set <- prediction_set[!is.na(predicted_performance)]
      if (!nrow(prediction_set)) next
      prediction_index <- prediction_index + 1L
      prediction_set[, `:=`(
        analysis_trust_id = provider_id,
        analysis_trust_name = provider$analysis_trust_name[target_index],
        origin_month = origin,
        target_month = target,
        actual_performance = provider$ae4h_performance[target_index],
        actual_ae4h_attendances_n = provider$ae4h_attendances_n[target_index],
        actual_ae4h_within_4h_n = provider$ae4h_within_4h_n[target_index],
        actual_ae4h_over_4h_n = provider$ae4h_over_4h_n[target_index]
      )]
      prediction_set[, c("forecast_month", "horizon_months") := NULL]
      provider_predictions[[prediction_index]] <- prediction_set
    }
    if (length(provider_predictions)) {
      pieces[[provider_index]] <- data.table::rbindlist(provider_predictions)
    }
  }
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(out)) stop("No provider rolling predictions were produced.")
  out[, `:=`(
    residual_pp = 100 * (actual_performance - predicted_performance),
    absolute_error_pp = abs(100 * (actual_performance - predicted_performance)),
    squared_error_pp = (100 * (actual_performance - predicted_performance))^2
  )]
  data.table::setorder(out, analysis_trust_id, target_month, model)
  out[]
}

score_provider_predictions <- function(rolling, config) {
  current_start <- parse_date_setting(config, "current_era_start")
  reference <- config$reference_model
  windows <- list(
    all_available = rolling,
    current_era = rolling[target_month >= current_start]
  )
  overall <- data.table::rbindlist(lapply(names(windows), function(window_name) {
    z <- windows[[window_name]]
    z[, .(
      provider_month_predictions_n = .N,
      providers_n = data.table::uniqueN(analysis_trust_id),
      mae_pp = mean(absolute_error_pp),
      rmse_pp = sqrt(mean(squared_error_pp)),
      bias_pp = mean(residual_pp),
      attendance_weighted_mae_pp = stats::weighted.mean(
        absolute_error_pp, actual_ae4h_attendances_n
      )
    ), by = model][order(rmse_pp)][, `:=`(
      evaluation_window = window_name,
      rmse_rank = seq_len(.N)
    )]
  }), use.names = TRUE)
  by_provider <- rolling[model == reference, .(
    prediction_months_n = .N,
    first_prediction_month = min(target_month),
    last_prediction_month = max(target_month),
    mae_pp = mean(absolute_error_pp),
    rmse_pp = sqrt(mean(squared_error_pp)),
    bias_pp = mean(residual_pp)
  ), by = .(analysis_trust_id, analysis_trust_name)]
  list(overall = overall[], by_provider = by_provider[])
}

make_provider_final_forecasts <- function(panel, config) {
  latest_month <- max(panel$calendar_month)
  end_month <- parse_date_setting(config, "forecast_end_month")
  future_dates <- future_month_sequence(latest_month, end_month)
  minimum_training <- parse_integer_setting(config, "minimum_training_months", 12L)
  current <- panel[
    calendar_month == latest_month & complete_submission & !is.na(ae4h_performance)
  ]
  pieces <- vector("list", nrow(current))
  for (i in seq_len(nrow(current))) {
    provider_id <- current$analysis_trust_id[i]
    train <- panel[
      analysis_trust_id == provider_id & calendar_month <= latest_month &
        complete_submission & !is.na(ae4h_performance)
    ]
    data.table::setorder(train, calendar_month)
    if (nrow(train) < minimum_training || max(train$calendar_month) != latest_month) next
    forecasts <- predict_provider_model_set(train, future_dates, config)
    forecasts[, `:=`(
      analysis_trust_id = provider_id,
      analysis_trust_name = current$analysis_trust_name[i],
      data_through_month = latest_month,
      training_months_n = nrow(train),
      identity_status = current$identity_status[i],
      source_org_codes = current$source_org_codes[i]
    )]
    pieces[[i]] <- forecasts
  }
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(out)) stop("No eligible current providers produced a final forecast.")
  data.table::setorder(out, analysis_trust_id, model, forecast_month)
  out[]
}

add_provider_one_step_intervals <- function(next_forecast, rolling, config) {
  current_start <- parse_date_setting(config, "current_era_start")
  minimum_provider <- parse_integer_setting(
    config, "minimum_provider_interval_residuals", 5L
  )
  minimum_pooled <- parse_integer_setting(
    config, "minimum_pooled_interval_residuals", 20L
  )
  residuals <- rolling[
    model == config$reference_model & target_month >= current_start
  ]
  pooled <- residuals$residual_pp
  pooled_method <- "pooled_provider_current_era_empirical"
  if (length(pooled) < minimum_pooled) {
    pooled <- rolling[model == config$reference_model, residual_pp]
    pooled_method <- "pooled_provider_all_available_empirical"
  }
  if (length(pooled) < minimum_pooled) {
    stop("Insufficient pooled provider residuals for empirical intervals.")
  }
  interval_rows <- lapply(seq_len(nrow(next_forecast)), function(i) {
    provider_id <- next_forecast$analysis_trust_id[i]
    calibration <- residuals[analysis_trust_id == provider_id, residual_pp]
    method <- "provider_specific_current_era_empirical"
    if (length(calibration) < minimum_provider) {
      calibration <- pooled
      method <- pooled_method
    }
    q <- stats::quantile(
      calibration / 100,
      probs = c(0.025, 0.10, 0.90, 0.975),
      names = FALSE,
      type = 8
    )
    point <- next_forecast$predicted_performance[i]
    data.table::data.table(
      lower_95 = bound_probability(point + q[1L]),
      lower_80 = bound_probability(point + q[2L]),
      upper_80 = bound_probability(point + q[3L]),
      upper_95 = bound_probability(point + q[4L]),
      interval_calibration_n = length(calibration),
      interval_method = method
    )
  })
  cbind(next_forecast, data.table::rbindlist(interval_rows))[]
}

add_provider_next_release_context <- function(next_forecast, panel) {
  if (!nrow(next_forecast)) return(next_forecast)
  target_month <- unique(next_forecast$forecast_month)
  data_month <- unique(next_forecast$data_through_month)
  if (length(target_month) != 1L || length(data_month) != 1L) {
    stop("Provider next-release output must contain one target and one data month.")
  }
  latest <- panel[
    calendar_month == data_month & complete_submission & !is.na(ae4h_performance),
    .(
      analysis_trust_id,
      latest_actual_performance = ae4h_performance,
      latest_ae4h_attendances_n = ae4h_attendances_n
    )
  ]
  prior <- panel[
    calendar_month == provider_previous_year_month(target_month) &
      complete_submission & !is.na(ae4h_performance),
    .(
      analysis_trust_id,
      same_month_last_year_performance = ae4h_performance
    )
  ]
  out <- merge(next_forecast, latest, by = "analysis_trust_id", all.x = TRUE)
  out <- merge(out, prior, by = "analysis_trust_id", all.x = TRUE)
  out[, `:=`(
    forecast_change_from_latest_pp = 100 * (
      predicted_performance - latest_actual_performance
    ),
    forecast_year_on_year_change_pp = 100 * (
      predicted_performance - same_month_last_year_performance
    )
  )]
  data.table::setorder(out, predicted_performance, analysis_trust_name)
  out[]
}

provider_current_sign_streak <- function(x, dates) {
  if (!length(x) || is.na(utils::tail(x, 1L)) || utils::tail(x, 1L) == 0) return(0L)
  final_sign <- sign(utils::tail(x, 1L))
  count <- 0L
  for (i in rev(seq_along(x))) {
    if (is.na(x[i]) || sign(x[i]) != final_sign) break
    if (i < length(x) && month_id(dates[i + 1L]) - month_id(dates[i]) != 1L) break
    count <- count + 1L
  }
  count
}

provider_release_surprise_history <- function(rolling, scorecard, config) {
  reference_model <- config$reference_model
  historical <- data.table::copy(rolling[model == reference_model])
  historical[, `:=`(
    forecast_evidence = "historically_simulated",
    forecast_created_at_utc = NA_character_
  )]
  version <- if (!is.null(config$release_forecast_version)) {
    config$release_forecast_version
  } else {
    "provider_all_ae4h_v1"
  }
  genuine <- data.table::data.table()
  if (nrow(scorecard)) {
    assert_columns(scorecard, c(
      "forecast_version", "data_through_month", "forecast_month",
      "analysis_trust_id", "analysis_trust_name", "model",
      "predicted_performance", "actual_performance", "forecast_status"
    ))
    genuine <- data.table::copy(scorecard[
      forecast_version == version & model == reference_model &
        forecast_status == "scored" & !is.na(actual_performance)
    ])
    if (nrow(genuine)) {
      genuine[, `:=`(
        origin_month = data.table::as.IDate(data_through_month),
        target_month = data.table::as.IDate(forecast_month),
        residual_pp = 100 * (actual_performance - predicted_performance),
        absolute_error_pp = abs(100 * (actual_performance - predicted_performance)),
        squared_error_pp = (100 * (actual_performance - predicted_performance))^2,
        forecast_evidence = "genuine_release_vintage"
      )]
      if (!"actual_ae4h_attendances_n" %in% names(genuine)) {
        genuine[, actual_ae4h_attendances_n := NA_real_]
      }
    }
  }
  keep <- c(
    "analysis_trust_id", "analysis_trust_name", "origin_month", "target_month",
    "model", "actual_performance", "predicted_performance",
    "actual_ae4h_attendances_n", "residual_pp", "absolute_error_pp",
    "squared_error_pp", "forecast_evidence", "forecast_created_at_utc"
  )
  genuine_rows <- if (nrow(genuine)) {
    genuine[, ..keep]
  } else {
    historical[0, ..keep]
  }
  combined <- data.table::rbindlist(
    list(historical[, ..keep], genuine_rows),
    use.names = TRUE,
    fill = TRUE
  )
  combined[, evidence_priority___ := data.table::fifelse(
    forecast_evidence == "genuine_release_vintage", 1L, 2L
  )]
  data.table::setorder(
    combined, analysis_trust_id, target_month, evidence_priority___
  )
  combined <- unique(combined, by = c("analysis_trust_id", "target_month"))
  combined[, evidence_priority___ := NULL]
  combined[]
}

provider_deviation_monitor <- function(surprises, config) {
  width <- parse_integer_setting(config, "persistent_window_months", 2L)
  materiality <- parse_numeric_setting(config, "persistent_materiality_pp", 0)
  direction_required <- parse_numeric_setting(
    config, "persistent_direction_share", 0.5, 1
  )
  calibration_months <- parse_integer_setting(
    config, "statistical_calibration_months", 12L
  )
  minimum_windows <- parse_integer_setting(
    config, "minimum_statistical_windows", 5L, calibration_months
  )
  reference <- surprises[model == config$reference_model]
  pieces <- lapply(split(reference, reference$analysis_trust_id), function(z) {
    z <- data.table::as.data.table(z)
    data.table::setorder(z, target_month)
    z[, `:=`(
      mean_residual_window_pp = NA_real_,
      direction_share_window = NA_real_,
      actual_mean_window = NA_real_,
      expected_mean_window = NA_real_,
      current_direction_streak_months = NA_integer_,
      statistical_lower_window_pp = NA_real_,
      statistical_upper_window_pp = NA_real_,
      statistically_unusual_window = NA,
      genuine_release_vintages_window = NA_integer_,
      simulated_vintages_window = NA_integer_,
      signal_evidence = NA_character_
    )]
    for (i in seq_len(nrow(z))) {
      z$current_direction_streak_months[i] <- provider_current_sign_streak(
        z$residual_pp[seq_len(i)], z$target_month[seq_len(i)]
      )
      if (i < width) next
      index <- (i - width + 1L):i
      expected_dates <- data.table::as.IDate(seq(
        as.Date(z$target_month[index[1L]]),
        by = "month",
        length.out = width
      ))
      if (!identical(as.character(expected_dates), as.character(z$target_month[index]))) next
      values <- z$residual_pp[index]
      z$mean_residual_window_pp[i] <- mean(values)
      z$direction_share_window[i] <- max(mean(values > 0), mean(values < 0))
      z$actual_mean_window[i] <- mean(z$actual_performance[index])
      z$expected_mean_window[i] <- mean(z$predicted_performance[index])
      genuine_n <- sum(
        z$forecast_evidence[index] == "genuine_release_vintage", na.rm = TRUE
      )
      z$genuine_release_vintages_window[i] <- genuine_n
      z$simulated_vintages_window[i] <- width - genuine_n
      z$signal_evidence[i] <- if (genuine_n == width) {
        "genuine_release_vintages"
      } else if (genuine_n > 0L) {
        "mixed_genuine_and_simulated"
      } else {
        "historically_simulated"
      }
      earlier <- seq_len(i - width)
      prior_values <- z$mean_residual_window_pp[earlier]
      prior_values <- utils::tail(prior_values[!is.na(prior_values)], calibration_months)
      if (length(prior_values) < minimum_windows) next
      q <- stats::quantile(
        prior_values, c(0.025, 0.975), names = FALSE, type = 8
      )
      z$statistical_lower_window_pp[i] <- q[1L]
      z$statistical_upper_window_pp[i] <- q[2L]
      z$statistically_unusual_window[i] <-
        z$mean_residual_window_pp[i] < q[1L] ||
        z$mean_residual_window_pp[i] > q[2L]
    }
    z[, `:=`(
      practically_material_window =
        !is.na(mean_residual_window_pp) &
        abs(mean_residual_window_pp) >= materiality,
      directionally_persistent_window =
        !is.na(direction_share_window) &
        direction_share_window >= direction_required
    )]
    z[, latest_error_same_direction :=
      !is.na(mean_residual_window_pp) & !is.na(residual_pp) & residual_pp != 0 &
      sign(residual_pp) == sign(mean_residual_window_pp)
    ]
    z[, core_signal := data.table::fcase(
      is.na(mean_residual_window_pp), "insufficient_consecutive_history",
      practically_material_window & directionally_persistent_window &
        mean_residual_window_pp > 0, "sustained_above_trajectory",
      practically_material_window & directionally_persistent_window &
        mean_residual_window_pp < 0, "sustained_below_trajectory",
      default = "within_sustained_threshold"
    )]
    z[, signal := data.table::fifelse(
      core_signal %in% c(
        "sustained_above_trajectory", "sustained_below_trajectory"
      ) & !latest_error_same_direction,
      "easing_toward_trajectory",
      core_signal
    )]
    z[, signal_status := data.table::fcase(
      signal %in% c("sustained_above_trajectory", "sustained_below_trajectory") &
        data.table::shift(signal, fill = "") == signal, "continuing_signal",
      signal %in% c("sustained_above_trajectory", "sustained_below_trajectory"),
        "new_signal",
      signal == "easing_toward_trajectory", "easing_signal",
      data.table::shift(signal, fill = "") %in% c(
        "sustained_above_trajectory", "sustained_below_trajectory"
      ), "returned_to_threshold",
      signal == "insufficient_consecutive_history", "insufficient_history",
      default = "stable_within_threshold"
    )]
    z[, review_priority := data.table::fcase(
      signal %in% c("sustained_above_trajectory", "sustained_below_trajectory") &
        statistically_unusual_window %in% TRUE, "high_review",
      signal %in% c("sustained_above_trajectory", "sustained_below_trajectory"),
        "review",
      default = "none"
    )]
    z
  })
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  data.table::setorder(out, analysis_trust_id, target_month)
  out[]
}

provider_fixed_origin_outlook <- function(panel, config) {
  width <- parse_integer_setting(config, "persistent_window_months", 2L)
  minimum_training <- parse_integer_setting(config, "minimum_training_months", 12L)
  latest_month <- max(panel$calendar_month)
  origin_month <- data.table::as.IDate(seq(
    as.Date(latest_month), by = "-1 month", length.out = width + 1L
  )[width + 1L])
  future_dates <- data.table::as.IDate(seq(
    as.Date(origin_month), by = "month", length.out = width + 1L
  )[-1L])
  eligible <- panel[
    calendar_month == latest_month & complete_submission & !is.na(ae4h_performance),
    unique(analysis_trust_id)
  ]
  pieces <- lapply(eligible, function(provider_id) {
    train <- panel[
      analysis_trust_id == provider_id & calendar_month <= origin_month &
        complete_submission & !is.na(ae4h_performance)
    ]
    data.table::setorder(train, calendar_month)
    if (nrow(train) < minimum_training ||
        !nrow(train) || max(train$calendar_month) != origin_month) {
      return(NULL)
    }
    forecast <- predict_provider_model_set(train, future_dates, config)[
      model == config$reference_model & !is.na(predicted_performance)
    ]
    actual <- panel[
      analysis_trust_id == provider_id & calendar_month %in% future_dates &
        complete_submission & !is.na(ae4h_performance),
      .(
        target_month = calendar_month,
        actual_performance = ae4h_performance,
        actual_ae4h_attendances_n = ae4h_attendances_n
      )
    ]
    out <- merge(
      forecast[, .(target_month = forecast_month, predicted_performance)],
      actual,
      by = "target_month"
    )
    if (nrow(out) != width) return(NULL)
    out[, `:=`(
      analysis_trust_id = provider_id,
      analysis_trust_name = latest_nonmissing_character(train$analysis_trust_name),
      fixed_origin_month = origin_month,
      residual_pp = 100 * (actual_performance - predicted_performance),
      context_evidence = "historically_simulated_fixed_origin"
    )]
    out
  })
  detail <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(detail)) {
    detail <- data.table::data.table(
      analysis_trust_id = character(),
      analysis_trust_name = character(),
      fixed_origin_month = data.table::as.IDate(character()),
      target_month = data.table::as.IDate(character()),
      predicted_performance = numeric(),
      actual_performance = numeric(),
      actual_ae4h_attendances_n = numeric(),
      residual_pp = numeric(),
      context_evidence = character()
    )
    return(list(detail = detail, summary = data.table::data.table()))
  }
  summary <- detail[, .(
    fixed_origin_month = unique(fixed_origin_month),
    fixed_origin_months_n = .N,
    fixed_origin_actual_average = mean(actual_performance),
    fixed_origin_expected_average = mean(predicted_performance),
    fixed_origin_gap_pp = mean(residual_pp),
    fixed_origin_direction_share = max(mean(residual_pp > 0), mean(residual_pp < 0)),
    fixed_origin_context_evidence = unique(context_evidence)
  ), by = analysis_trust_id]
  data.table::setorder(detail, analysis_trust_id, target_month)
  list(detail = detail[], summary = summary[])
}

make_provider_watchlist <- function(
    panel, next_forecast, deviations, fixed_origin_summary, config) {
  latest_month <- max(panel$calendar_month)
  latest <- panel[
    calendar_month == latest_month & complete_submission & !is.na(ae4h_performance),
    .(
      analysis_trust_id,
      analysis_trust_name,
      data_through_month = calendar_month,
      latest_actual_performance = ae4h_performance,
      latest_ae4h_attendances_n = ae4h_attendances_n,
      identity_status,
      source_org_codes
    )
  ]
  forecast <- next_forecast[, .(
    analysis_trust_id,
    forecast_month,
    predicted_performance,
    lower_80,
    upper_80,
    lower_95,
    upper_95,
    interval_calibration_n,
    interval_method,
    training_months_n
  )]
  current_deviation <- deviations[target_month == latest_month, .(
    analysis_trust_id,
    six_month_actual_average = actual_mean_window,
    six_month_expected_average = expected_mean_window,
    six_month_gap_to_trajectory_pp = mean_residual_window_pp,
    six_month_direction_share = direction_share_window,
    current_direction_streak_months,
    statistical_lower_6m_pp = statistical_lower_window_pp,
    statistical_upper_6m_pp = statistical_upper_window_pp,
    statistically_unusual_6m = statistically_unusual_window,
    practically_material_6m = practically_material_window,
    directionally_persistent_6m = directionally_persistent_window,
    latest_error_same_direction,
    genuine_release_vintages_6m = genuine_release_vintages_window,
    simulated_vintages_6m = simulated_vintages_window,
    signal_evidence,
    signal,
    signal_status,
    review_priority
  )]
  out <- merge(latest, forecast, by = "analysis_trust_id", all.x = TRUE)
  out <- merge(out, current_deviation, by = "analysis_trust_id", all.x = TRUE)
  if (nrow(fixed_origin_summary)) {
    out <- merge(out, fixed_origin_summary, by = "analysis_trust_id", all.x = TRUE)
  } else {
    out[, `:=`(
      fixed_origin_month = data.table::as.IDate(NA),
      fixed_origin_months_n = NA_integer_,
      fixed_origin_actual_average = NA_real_,
      fixed_origin_expected_average = NA_real_,
      fixed_origin_gap_pp = NA_real_,
      fixed_origin_direction_share = NA_real_,
      fixed_origin_context_evidence = NA_character_
    )]
  }
  out[is.na(signal), `:=`(
    signal = "insufficient_consecutive_history",
    signal_status = "insufficient_history",
    review_priority = "none"
  )]
  out[, `:=`(
    forecast_change_from_latest_pp = 100 * (
      predicted_performance - latest_actual_performance
    ),
    forecast_eligibility = data.table::fifelse(
      is.na(predicted_performance), "not_eligible", "eligible"
    )
  )]
  priority_order <- c(high_review = 1L, review = 2L, none = 3L)
  out[, `:=`(
    priority_sort___ = priority_order[review_priority],
    gap_sort___ = abs(six_month_gap_to_trajectory_pp)
  )]
  data.table::setorder(
    out, priority_sort___, -gap_sort___, analysis_trust_name
  )
  out[, c("priority_sort___", "gap_sort___") := NULL]
  out[]
}

make_provider_release_archive_rows <- function(watchlist, config) {
  version <- if (!is.null(config$release_forecast_version)) {
    config$release_forecast_version
  } else {
    "provider_all_ae4h_v1"
  }
  watchlist[, .(
    forecast_version = version,
    data_through_month,
    forecast_month,
    analysis_trust_id,
    analysis_trust_name,
    model = config$reference_model,
    predicted_performance,
    lower_80,
    upper_80,
    lower_95,
    upper_95,
    latest_actual_performance,
    interval_calibration_n,
    interval_method,
    identity_status,
    source_org_codes
  )]
}

make_provider_signal_archive_rows <- function(watchlist, config) {
  version <- if (!is.null(config$release_forecast_version)) {
    config$release_forecast_version
  } else {
    "provider_all_ae4h_v1"
  }
  watchlist[, .(
    signal_version = version,
    data_through_month,
    analysis_trust_id,
    analysis_trust_name,
    latest_actual_performance,
    latest_ae4h_attendances_n,
    six_month_actual_average,
    six_month_expected_average,
    six_month_gap_to_trajectory_pp,
    six_month_direction_share,
    current_direction_streak_months,
    statistically_unusual_6m,
    practically_material_6m,
    directionally_persistent_6m,
    latest_error_same_direction,
    genuine_release_vintages_6m,
    simulated_vintages_6m,
    signal_evidence,
    signal,
    signal_status,
    review_priority,
    fixed_origin_month,
    fixed_origin_months_n,
    fixed_origin_actual_average,
    fixed_origin_expected_average,
    fixed_origin_gap_pp,
    fixed_origin_direction_share,
    fixed_origin_context_evidence,
    identity_status,
    source_org_codes
  )]
}

score_provider_release_archive <- function(archive, panel) {
  if (!nrow(archive)) return(data.table::data.table())
  x <- data.table::copy(archive)
  x[, forecast_month := data.table::as.IDate(forecast_month)]
  actual <- panel[complete_submission & !is.na(ae4h_performance), .(
    analysis_trust_id,
    forecast_month = calendar_month,
    actual_performance = ae4h_performance,
    actual_ae4h_attendances_n = ae4h_attendances_n
  )]
  out <- merge(
    x, actual, by = c("analysis_trust_id", "forecast_month"), all.x = TRUE
  )
  out[, `:=`(
    residual_pp = 100 * (actual_performance - predicted_performance),
    absolute_error_pp = abs(100 * (actual_performance - predicted_performance)),
    forecast_status = data.table::fifelse(
      is.na(actual_performance), "awaiting_release", "scored"
    )
  )]
  data.table::setorder(out, forecast_month, analysis_trust_name)
  out[]
}
