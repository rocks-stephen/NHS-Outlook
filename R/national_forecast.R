month_number <- function(x) as.integer(format(as.Date(x), "%m"))

month_id <- function(x) {
  d <- as.Date(x)
  as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
}

bound_probability <- function(x, epsilon = 1e-6) {
  pmin(1 - epsilon, pmax(epsilon, as.numeric(x)))
}

logit_probability <- function(x) stats::qlogis(bound_probability(x))

parse_integer_setting <- function(config, key, minimum = -Inf, maximum = Inf) {
  value <- suppressWarnings(as.integer(config[[key]]))
  if (length(value) != 1L || is.na(value) || value < minimum || value > maximum) {
    stop("Invalid integer setting '", key, "': ", config[[key]])
  }
  value
}

parse_numeric_setting <- function(config, key, minimum = -Inf, maximum = Inf) {
  value <- suppressWarnings(as.numeric(config[[key]]))
  if (length(value) != 1L || is.na(value) || value < minimum || value > maximum) {
    stop("Invalid numeric setting '", key, "': ", config[[key]])
  }
  value
}

parse_date_setting <- function(config, key) {
  value <- data.table::as.IDate(config[[key]])
  if (length(value) != 1L || is.na(value)) stop("Invalid date setting '", key, "'.")
  value
}

parse_integer_vector_setting <- function(config, key) {
  value <- suppressWarnings(as.integer(trimws(strsplit(config[[key]], ",", fixed = TRUE)[[1L]])))
  if (!length(value) || anyNA(value)) stop("Invalid integer vector setting '", key, "'.")
  value
}

parse_character_vector_setting <- function(config, key) {
  value <- trimws(strsplit(config[[key]], ",", fixed = TRUE)[[1L]])
  value[nzchar(value)]
}

validate_national_model_data <- function(x) {
  required <- c(
    "calendar_month", "ae4h_attendances_n", "ae4h_within_4h_n",
    "ae4h_over_4h_n", "ae4h_performance", "source_method",
    "national_comparability_era"
  )
  assert_columns(x, required)
  if (anyNA(x[, ..required])) stop("National modelling inputs contain missing required values.")
  data.table::setorder(x, calendar_month)
  if (anyDuplicated(x$calendar_month)) stop("National modelling inputs contain duplicate months.")
  expected <- data.table::as.IDate(seq(
    as.Date(min(x$calendar_month)), as.Date(max(x$calendar_month)), by = "month"
  ))
  if (!identical(as.character(expected), as.character(x$calendar_month))) {
    stop("National modelling inputs are not a consecutive monthly series.")
  }
  if (any(x$ae4h_attendances_n <= 0)) stop("National all-types denominators must be positive.")
  if (any(abs(
    x$ae4h_attendances_n - x$ae4h_within_4h_n - x$ae4h_over_4h_n
  ) > 1e-8)) stop("National numerator and denominator identities fail.")
  calculated <- x$ae4h_within_4h_n / x$ae4h_attendances_n
  if (any(abs(calculated - x$ae4h_performance) > 1e-8)) {
    stop("National performance does not match the retained counts.")
  }
  invisible(TRUE)
}

future_month_sequence <- function(last_observed, end_month) {
  first_future <- seq(as.Date(last_observed), by = "month", length.out = 2L)[2L]
  end_month <- as.Date(end_month)
  if (end_month < first_future) stop("Forecast end month precedes the first forecast month.")
  data.table::as.IDate(seq(first_future, end_month, by = "month"))
}

predict_seasonal_naive <- function(train, future_dates) {
  vapply(future_dates, function(target) {
    candidates <- train[month_number(calendar_month) == month_number(target)]
    if (!nrow(candidates)) return(NA_real_)
    candidates$ae4h_performance[nrow(candidates)]
  }, numeric(1))
}

predict_seasonal_mean <- function(train, future_dates, years = 3L) {
  vapply(future_dates, function(target) {
    candidates <- train[month_number(calendar_month) == month_number(target)]
    if (!nrow(candidates)) return(NA_real_)
    mean(utils::tail(candidates$ae4h_performance, years))
  }, numeric(1))
}

predict_seasonal_drift <- function(train, future_dates, comparison_months, damping) {
  if (nrow(train) < 24L) return(rep(NA_real_, length(future_dates)))
  z <- logit_probability(train$ae4h_performance)
  year_on_year <- z[13:length(z)] - z[1:(length(z) - 12L)]
  annual_drift <- mean(utils::tail(year_on_year, comparison_months))
  vapply(future_dates, function(target) {
    candidates <- which(month_number(train$calendar_month) == month_number(target))
    if (!length(candidates)) return(NA_real_)
    base_index <- utils::tail(candidates, 1L)
    cycles <- as.integer((month_id(target) - month_id(train$calendar_month[base_index])) / 12L)
    if (cycles < 1L) stop("Seasonal drift requires a future month.")
    drift_multiplier <- if (abs(damping - 1) < 1e-12) {
      cycles
    } else {
      (1 - damping^cycles) / (1 - damping)
    }
    stats::plogis(z[base_index] + annual_drift * drift_multiplier)
  }, numeric(1))
}

predict_count_glm_trend <- function(train, future_dates, window_months) {
  if (nrow(train) < window_months) return(rep(NA_real_, length(future_dates)))
  fit_data <- data.table::copy(utils::tail(train, window_months))
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

make_dynamic_xreg <- function(dates, origin_date) {
  month <- month_number(dates)
  season <- vapply(2:12, function(value) as.numeric(month == value), numeric(length(month)))
  colnames(season) <- paste0("month_", sprintf("%02d", 2:12))
  cbind(trend = month_id(dates) - month_id(origin_date), season)
}

predict_dynamic_ar1 <- function(train, future_dates, window_months = 84L) {
  if (nrow(train) < window_months) return(rep(NA_real_, length(future_dates)))
  fit_data <- data.table::copy(utils::tail(train, window_months))
  origin_date <- max(fit_data$calendar_month)
  fit <- stats::arima(
    logit_probability(fit_data$ae4h_performance),
    order = c(1L, 0L, 0L),
    xreg = make_dynamic_xreg(fit_data$calendar_month, origin_date),
    include.mean = TRUE,
    method = "ML"
  )
  prediction <- stats::predict(
    fit,
    n.ahead = length(future_dates),
    newxreg = make_dynamic_xreg(future_dates, origin_date)
  )$pred
  bound_probability(stats::plogis(as.numeric(prediction)))
}

predict_shared_season_current_trend <- function(train, future_dates, config) {
  pre_start <- parse_date_setting(config, "pre_crs_season_start")
  pre_end <- parse_date_setting(config, "pre_crs_season_end")
  current_start <- parse_date_setting(config, "current_era_start")
  minimum_current <- parse_integer_setting(config, "minimum_current_months", 12L)
  pre <- train[calendar_month >= pre_start & calendar_month <= pre_end]
  current <- train[calendar_month >= current_start]
  if (nrow(pre) < 36L || nrow(current) < minimum_current) {
    return(rep(NA_real_, length(future_dates)))
  }
  fit_data <- data.table::rbindlist(list(pre, current), use.names = TRUE)
  pre_origin <- month_id(min(pre$calendar_month))
  current_origin <- month_id(current_start)
  fit_data[, current_era := as.integer(calendar_month >= current_start)]
  fit_data[, pre_trend := data.table::fifelse(
    current_era == 0L, month_id(calendar_month) - pre_origin, 0
  )]
  fit_data[, current_trend := data.table::fifelse(
    current_era == 1L, month_id(calendar_month) - current_origin, 0
  )]
  fit_data[, month_factor := factor(month_number(calendar_month), levels = 1:12)]
  fit <- stats::glm(
    cbind(ae4h_within_4h_n, ae4h_over_4h_n) ~
      current_era + pre_trend + current_trend + month_factor,
    family = stats::quasibinomial(link = "logit"),
    data = fit_data
  )
  new_data <- data.frame(
    current_era = 1L,
    pre_trend = 0,
    current_trend = month_id(future_dates) - current_origin,
    month_factor = factor(month_number(future_dates), levels = 1:12)
  )
  bound_probability(stats::predict(fit, newdata = new_data, type = "response"))
}

predict_national_model <- function(model_id, train, future_dates, config) {
  comparison_months <- parse_integer_setting(config, "drift_comparison_months", 1L)
  damping <- parse_numeric_setting(config, "drift_damping", 0, 1)
  switch(
    model_id,
    seasonal_naive = predict_seasonal_naive(train, future_dates),
    seasonal_mean_3y = predict_seasonal_mean(train, future_dates, years = 3L),
    seasonal_drift_damped = predict_seasonal_drift(
      train, future_dates, comparison_months, damping
    ),
    count_glm_trend_36 = predict_count_glm_trend(train, future_dates, 36L),
    count_glm_trend_60 = predict_count_glm_trend(train, future_dates, 60L),
    count_glm_trend_all = predict_count_glm_trend(train, future_dates, nrow(train)),
    dynamic_ar1_trend_84 = predict_dynamic_ar1(train, future_dates, 84L),
    shared_season_current_trend = predict_shared_season_current_trend(
      train, future_dates, config
    ),
    reference_ensemble = {
      components <- cbind(
        predict_seasonal_naive(train, future_dates),
        predict_seasonal_drift(train, future_dates, comparison_months, damping),
        predict_shared_season_current_trend(train, future_dates, config)
      )
      if (anyNA(components)) rep(NA_real_, length(future_dates)) else rowMeans(components)
    },
    stop("Unknown national model: ", model_id)
  )
}

rolling_national_predictions <- function(x, config) {
  minimum_training <- parse_integer_setting(config, "minimum_training_months", 24L)
  maximum_horizon <- parse_integer_setting(config, "maximum_backtest_horizon", 1L)
  model_ids <- parse_character_vector_setting(config, "candidate_models")
  if (nrow(x) <= minimum_training) stop("Not enough national months for rolling evaluation.")
  predictions <- list()
  failures <- list()
  prediction_index <- 0L
  failure_index <- 0L
  for (origin in seq.int(minimum_training, nrow(x) - 1L)) {
    target_rows <- seq.int(origin + 1L, min(nrow(x), origin + maximum_horizon))
    future_dates <- x$calendar_month[target_rows]
    train <- x[seq_len(origin)]
    for (model_id in model_ids) {
      result <- tryCatch(
        predict_national_model(model_id, train, future_dates, config),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        failure_index <- failure_index + 1L
        failures[[failure_index]] <- data.table::data.table(
          origin_month = x$calendar_month[origin],
          model = model_id,
          error = conditionMessage(result)
        )
        next
      }
      if (length(result) != length(target_rows)) {
        stop("Model ", model_id, " returned the wrong number of predictions.")
      }
      keep <- !is.na(result)
      if (!any(keep)) next
      prediction_index <- prediction_index + 1L
      predictions[[prediction_index]] <- data.table::data.table(
        model = model_id,
        origin_month = x$calendar_month[origin],
        target_month = x$calendar_month[target_rows][keep],
        horizon_months = seq_along(target_rows)[keep],
        actual_performance = x$ae4h_performance[target_rows][keep],
        predicted_performance = result[keep],
        actual_ae4h_attendances_n = x$ae4h_attendances_n[target_rows][keep],
        actual_ae4h_within_4h_n = x$ae4h_within_4h_n[target_rows][keep],
        actual_ae4h_over_4h_n = x$ae4h_over_4h_n[target_rows][keep]
      )
    }
  }
  out <- data.table::rbindlist(predictions, use.names = TRUE, fill = TRUE)
  out[, `:=`(
    residual_pp = 100 * (actual_performance - predicted_performance),
    absolute_error_pp = abs(100 * (actual_performance - predicted_performance)),
    squared_error_pp = (100 * (actual_performance - predicted_performance))^2
  )]
  failure_table <- data.table::rbindlist(failures, use.names = TRUE, fill = TRUE)
  list(predictions = out, failures = failure_table)
}

score_national_predictions <- function(rolling, config) {
  current_start <- parse_date_setting(config, "current_era_start")
  selection_start <- parse_date_setting(config, "selection_target_start")
  selection_horizons <- parse_integer_vector_setting(config, "selection_horizons")
  windows <- list(
    all_available = rolling,
    current_era_targets = rolling[target_month >= current_start],
    primary_selection = rolling[
      target_month >= selection_start & horizon_months %in% selection_horizons
    ]
  )
  by_horizon <- data.table::rbindlist(lapply(names(windows), function(window_name) {
    z <- windows[[window_name]]
    z[, .(
      n_predictions = .N,
      n_origins = data.table::uniqueN(origin_month),
      mae_pp = mean(absolute_error_pp),
      rmse_pp = sqrt(mean(squared_error_pp)),
      bias_pp = mean(residual_pp)
    ), by = .(model, horizon_months)][, evaluation_window := window_name]
  }), use.names = TRUE)
  pooled <- data.table::rbindlist(lapply(names(windows), function(window_name) {
    z <- windows[[window_name]]
    z[, .(
      n_predictions = .N,
      n_origins = data.table::uniqueN(origin_month),
      mae_pp = mean(absolute_error_pp),
      rmse_pp = sqrt(mean(squared_error_pp)),
      bias_pp = mean(residual_pp)
    ), by = model][order(rmse_pp)][, `:=`(
      evaluation_window = window_name,
      rmse_rank = seq_len(.N)
    )]
  }), use.names = TRUE)
  list(by_horizon = by_horizon[], pooled = pooled[])
}

make_final_national_forecasts <- function(x, config) {
  forecast_end <- parse_date_setting(config, "forecast_end_month")
  future_dates <- future_month_sequence(max(x$calendar_month), forecast_end)
  model_ids <- parse_character_vector_setting(config, "candidate_models")
  data.table::rbindlist(lapply(model_ids, function(model_id) {
    prediction <- predict_national_model(model_id, x, future_dates, config)
    data.table::data.table(
      model = model_id,
      forecast_month = future_dates,
      horizon_months = seq_along(future_dates),
      predicted_performance = prediction
    )
  }), use.names = TRUE)
}

horizon_band <- function(horizon) {
  data.table::fcase(
    horizon <= 3L, "01_03",
    horizon <= 6L, "04_06",
    horizon <= 12L, "07_12",
    horizon <= 24L, "13_24",
    default = "25_plus"
  )
}

add_empirical_intervals <- function(reference_forecast, rolling, config) {
  reference_model <- config$reference_model
  current_start <- parse_date_setting(config, "current_era_start")
  minimum_n <- parse_integer_setting(config, "minimum_interval_residuals", 5L)
  residuals <- rolling[model == reference_model & target_month >= current_start]
  if (!nrow(residuals)) stop("No current-era residuals exist for the reference model.")
  residuals[, interval_band := horizon_band(horizon_months)]
  out <- data.table::copy(reference_forecast)
  out[, interval_band := horizon_band(horizon_months)]
  quantile_rows <- lapply(seq_len(nrow(out)), function(i) {
    calibration <- residuals[interval_band == out$interval_band[i]]
    method <- paste0("current_era_empirical_", out$interval_band[i])
    if (nrow(calibration) < minimum_n) {
      calibration <- if (out$horizon_months[i] >= 13L) {
        residuals[horizon_months >= 13L]
      } else {
        residuals
      }
      method <- if (out$horizon_months[i] >= 13L) {
        "current_era_empirical_pooled_13_plus"
      } else {
        "current_era_empirical_all_horizons"
      }
    }
    if (nrow(calibration) < minimum_n) {
      stop("Insufficient reference-model residuals for empirical intervals.")
    }
    q <- stats::quantile(
      calibration$residual_pp / 100,
      probs = c(0.025, 0.10, 0.90, 0.975),
      names = FALSE,
      type = 8
    )
    data.table::data.table(
      lower_95 = bound_probability(out$predicted_performance[i] + q[1L]),
      lower_80 = bound_probability(out$predicted_performance[i] + q[2L]),
      upper_80 = bound_probability(out$predicted_performance[i] + q[3L]),
      upper_95 = bound_probability(out$predicted_performance[i] + q[4L]),
      interval_calibration_n = nrow(calibration),
      interval_method = method
    )
  })
  cbind(out, data.table::rbindlist(quantile_rows))[]
}

trailing_mean <- function(x, width) {
  if (length(x) < width) return(rep(NA_real_, length(x)))
  data.table::frollmean(x, n = width, align = "right", fill = NA_real_)
}

trailing_direction_share <- function(x, width) {
  vapply(seq_along(x), function(i) {
    if (i < width) return(NA_real_)
    values <- x[(i - width + 1L):i]
    max(mean(values > 0), mean(values < 0))
  }, numeric(1))
}

prior_empirical_flag <- function(values, calibration_months, minimum_n) {
  lower <- upper <- rep(NA_real_, length(values))
  unusual <- rep(NA, length(values))
  for (i in seq_along(values)) {
    if (is.na(values[i]) || i <= 1L) next
    prior <- values[seq_len(i - 1L)]
    prior <- utils::tail(prior[!is.na(prior)], calibration_months)
    if (length(prior) < minimum_n) next
    q <- stats::quantile(prior, c(0.025, 0.975), names = FALSE, type = 8)
    lower[i] <- q[1L]
    upper[i] <- q[2L]
    unusual[i] <- values[i] < lower[i] || values[i] > upper[i]
  }
  list(lower = lower, upper = upper, unusual = unusual)
}

national_deviation_monitor <- function(rolling, config) {
  reference_model <- config$reference_model
  materiality <- parse_numeric_setting(config, "materiality_threshold_pp", 0)
  direction_required <- parse_numeric_setting(config, "persistent_direction_share", 0.5, 1)
  calibration_months <- parse_integer_setting(config, "statistical_calibration_months", 12L)
  minimum_n <- parse_integer_setting(
    config, "minimum_statistical_residuals", 5L, calibration_months
  )
  out <- data.table::copy(rolling[model == reference_model & horizon_months == 1L])
  data.table::setorder(out, target_month)
  one_flag <- prior_empirical_flag(out$residual_pp, calibration_months, minimum_n)
  out[, `:=`(
    statistical_lower_1m_pp = one_flag$lower,
    statistical_upper_1m_pp = one_flag$upper,
    statistically_unusual_1m = one_flag$unusual,
    practically_material_1m = abs(residual_pp) >= materiality
  )]
  for (width in c(3L, 6L)) {
    mean_name <- paste0("mean_residual_", width, "m_pp")
    direction_name <- paste0("direction_share_", width, "m")
    material_name <- paste0("practically_material_", width, "m")
    unusual_name <- paste0("statistically_unusual_", width, "m")
    lower_name <- paste0("statistical_lower_", width, "m_pp")
    upper_name <- paste0("statistical_upper_", width, "m_pp")
    out[, (mean_name) := trailing_mean(residual_pp, width)]
    out[, (direction_name) := trailing_direction_share(residual_pp, width)]
    flag <- prior_empirical_flag(out[[mean_name]], calibration_months, minimum_n)
    out[, (lower_name) := flag$lower]
    out[, (upper_name) := flag$upper]
    out[, (unusual_name) := flag$unusual]
    out[, (material_name) :=
      !is.na(get(mean_name)) &
      abs(get(mean_name)) >= materiality &
      get(direction_name) >= direction_required
    ]
  }
  out[]
}
