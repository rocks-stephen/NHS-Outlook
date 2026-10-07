core_config_number <- function(config_row, column, minimum = -Inf, maximum = Inf) {
  value <- suppressWarnings(as.numeric(config_row[[column]][1L]))
  if (!is.finite(value) || value < minimum || value > maximum) {
    stop("Invalid core model setting '", column, "' for ", config_row$metric_id[1L], ".")
  }
  value
}

core_config_integer <- function(config_row, column, minimum = -Inf, maximum = Inf) {
  value <- suppressWarnings(as.integer(config_row[[column]][1L]))
  if (is.na(value) || value < minimum || value > maximum) {
    stop("Invalid core model setting '", column, "' for ", config_row$metric_id[1L], ".")
  }
  value
}

core_config_character <- function(config_row, column, allowed = NULL) {
  value <- trimws(as.character(config_row[[column]][1L]))
  if (length(value) != 1L || is.na(value) || !nzchar(value) ||
      (!is.null(allowed) && !value %in% allowed)) {
    stop("Invalid core model setting '", column, "' for ", config_row$metric_id[1L], ".")
  }
  value
}

core_transform <- function(value, unit) {
  if (unit == "proportion") return(logit_probability(value))
  if (unit %in% c("minutes", "count")) return(log(pmax(as.numeric(value), 1e-6)))
  as.numeric(value)
}

core_inverse_transform <- function(value, unit) {
  if (unit == "proportion") return(bound_probability(stats::plogis(value)))
  if (unit %in% c("minutes", "count")) return(exp(value))
  as.numeric(value)
}

core_bound_value <- function(value, unit) {
  if (unit == "proportion") return(pmin(1, pmax(0, as.numeric(value))))
  if (unit %in% c("minutes", "count")) return(pmax(0, as.numeric(value)))
  as.numeric(value)
}

core_contiguous_tail <- function(x) {
  if (!nrow(x)) return(x)
  z <- data.table::copy(x[complete_submission == TRUE & is.finite(value)])
  data.table::setorder(z, calendar_month)
  if (!nrow(z)) return(z)
  breaks <- which(diff(month_id(z$calendar_month)) != 1L)
  if (length(breaks)) z <- z[(utils::tail(breaks, 1L) + 1L):.N]
  z[]
}

core_predict_seasonal_naive <- function(train, future_dates) {
  vapply(future_dates, function(target) {
    candidates <- train[month_number(calendar_month) == month_number(target)]
    if (!nrow(candidates)) NA_real_ else candidates$value[nrow(candidates)]
  }, numeric(1))
}

core_ensemble_component_models <- function() {
  c(
    "recent_level_seasonal",
    "seasonal_drift_damped",
    "recent_trend_seasonal"
  )
}

core_predict_recent_level_seasonal <- function(train, future_dates, unit,
                                               seasonal_change_years) {
  z <- data.table::copy(train)
  data.table::setorder(z, calendar_month)
  if (nrow(z) < 13L) return(rep(NA_real_, length(future_dates)))
  z[, transformed_value___ := core_transform(value, unit)]
  z[, `:=`(
    prior_month_id___ = data.table::shift(month_id(calendar_month)),
    seasonal_change___ = transformed_value___ -
      data.table::shift(transformed_value___),
    target_month_number___ = month_number(calendar_month)
  )]
  changes <- z[
    month_id(calendar_month) - prior_month_id___ == 1L &
      is.finite(seasonal_change___),
    .(calendar_month, target_month_number___, seasonal_change___)
  ]
  if (!nrow(changes)) return(rep(NA_real_, length(future_dates)))
  data.table::setorder(changes, target_month_number___, calendar_month)
  changes <- changes[, utils::tail(.SD, seasonal_change_years),
                     by = target_month_number___]
  seasonal_pattern <- changes[, .(
    seasonal_change = stats::median(seasonal_change___, na.rm = TRUE)
  ), by = target_month_number___]
  if (data.table::uniqueN(seasonal_pattern$target_month_number___) < 12L) {
    return(rep(NA_real_, length(future_dates)))
  }
  # Month-to-month seasonal changes should describe a cycle, not introduce a
  # second long-run trend. Centre the 12 changes so that they sum to zero.
  seasonal_pattern[, seasonal_change :=
    seasonal_change - mean(seasonal_change)]
  origin_month <- max(z$calendar_month)
  origin_value <- z[calendar_month == origin_month, transformed_value___][1L]
  vapply(future_dates, function(target) {
    horizon <- month_id(target) - month_id(origin_month)
    if (!is.finite(horizon) || horizon < 1L) return(NA_real_)
    steps <- data.table::as.IDate(seq(
      as.Date(origin_month), by = "month", length.out = horizon + 1L
    )[-1L])
    step_changes <- seasonal_pattern[
      match(month_number(steps), target_month_number___), seasonal_change
    ]
    if (length(step_changes) != horizon || any(!is.finite(step_changes))) {
      return(NA_real_)
    }
    core_inverse_transform(origin_value + sum(step_changes), unit)
  }, numeric(1))
}

core_predict_seasonal_drift <- function(train, future_dates, unit,
                                        comparison_months, damping) {
  if (nrow(train) < 24L) return(rep(NA_real_, length(future_dates)))
  transformed <- core_transform(train$value, unit)
  year_on_year <- transformed[13:length(transformed)] -
    transformed[1:(length(transformed) - 12L)]
  annual_drift <- mean(utils::tail(year_on_year, comparison_months), na.rm = TRUE)
  vapply(future_dates, function(target) {
    candidates <- which(month_number(train$calendar_month) == month_number(target))
    if (!length(candidates)) return(NA_real_)
    base_index <- utils::tail(candidates, 1L)
    cycles <- as.integer(
      (month_id(target) - month_id(train$calendar_month[base_index])) / 12L
    )
    if (cycles < 1L) return(NA_real_)
    drift_multiplier <- if (abs(damping - 1) < 1e-12) {
      cycles
    } else {
      (1 - damping^cycles) / (1 - damping)
    }
    core_inverse_transform(
      transformed[base_index] + annual_drift * drift_multiplier, unit
    )
  }, numeric(1))
}

core_predict_recent_trend <- function(train, future_dates, unit, window_months,
                                      trend_damping) {
  fit_data <- data.table::copy(utils::tail(train, window_months))
  if (nrow(fit_data) < 18L ||
      data.table::uniqueN(month_number(fit_data$calendar_month)) < 12L) {
    return(rep(NA_real_, length(future_dates)))
  }
  origin_month <- max(fit_data$calendar_month)
  fit_data[, `:=`(
    transformed_value = core_transform(value, unit),
    trend = month_id(calendar_month) - month_id(origin_month),
    month_factor = factor(month_number(calendar_month), levels = 1:12)
  )]
  fit <- stats::lm(transformed_value ~ trend + month_factor, data = fit_data)
  horizon <- month_id(future_dates) - month_id(origin_month)
  effective_horizon <- if (abs(trend_damping - 1) < 1e-12) {
    horizon
  } else {
    (1 - trend_damping^horizon) / (1 - trend_damping)
  }
  new_data <- data.frame(
    trend = effective_horizon,
    month_factor = factor(month_number(future_dates), levels = 1:12)
  )
  core_inverse_transform(as.numeric(stats::predict(fit, newdata = new_data)), unit)
}

core_equal_ensemble_weights <- function() {
  models <- core_ensemble_component_models()
  data.table::data.table(
    model = models,
    ensemble_weight = rep(1 / length(models), length(models)),
    component_rmse = NA_real_,
    weighting_target_months = 0L,
    weighting_method = "equal_weight_insufficient_history"
  )
}

core_estimate_ensemble_weights <- function(predictions, config_row,
                                           before_target = NULL) {
  models <- core_ensemble_component_models()
  z <- data.table::copy(predictions[
    model %in% models & is.finite(error_native)
  ])
  if (!is.null(before_target)) {
    z <- z[target_month < data.table::as.IDate(before_target)]
  }
  minimum_months <- core_config_integer(
    config_row, "ensemble_weight_minimum_months", 1L
  )
  target_months <- if (nrow(z)) data.table::uniqueN(z$target_month) else 0L
  scores <- z[, .(
    component_rmse = sqrt(mean(error_native^2))
  ), by = model]
  if (
    target_months < minimum_months ||
      !setequal(scores$model[is.finite(scores$component_rmse)], models)
  ) {
    out <- core_equal_ensemble_weights()
    out[, weighting_target_months := as.integer(target_months)]
    return(out[])
  }
  scores[, model_order___ := match(model, models)]
  data.table::setorder(scores, model_order___)
  scores[, model_order___ := NULL]
  positive_rmse <- scores$component_rmse[
    is.finite(scores$component_rmse) & scores$component_rmse > 0
  ]
  floor_value <- if (length(positive_rmse)) {
    max(min(positive_rmse) * 0.05, .Machine$double.eps)
  } else {
    .Machine$double.eps
  }
  scores[, raw_weight___ := 1 / pmax(component_rmse, floor_value)]
  scores[, raw_weight___ := raw_weight___ / sum(raw_weight___)]
  shrinkage <- core_config_number(
    config_row, "ensemble_equal_weight_shrinkage", 0, 1
  )
  scores[, ensemble_weight :=
    (1 - shrinkage) * raw_weight___ + shrinkage / .N]
  scores[, `:=`(
    weighting_target_months = as.integer(target_months),
    weighting_method = "inverse_rmse_shrunk_toward_equal"
  )]
  scores[, raw_weight___ := NULL]
  scores[, .(
    model, ensemble_weight, component_rmse,
    weighting_target_months, weighting_method
  )]
}

core_weighted_component_mean <- function(components, ensemble_weights,
                                         minimum_components) {
  models <- core_ensemble_component_models()
  weights <- core_equal_ensemble_weights()
  if (!is.null(ensemble_weights) && nrow(ensemble_weights)) {
    supplied <- data.table::copy(ensemble_weights)[
      model %in% models & is.finite(ensemble_weight)
    ]
    if (setequal(supplied$model, models)) weights <- supplied
  }
  weight_vector <- weights$ensemble_weight[match(models, weights$model)]
  vapply(seq_len(nrow(components)), function(i) {
    available <- is.finite(components[i, ]) & is.finite(weight_vector) &
      weight_vector > 0
    if (sum(available) < minimum_components) return(NA_real_)
    stats::weighted.mean(
      components[i, available], weight_vector[available], na.rm = TRUE
    )
  }, numeric(1))
}

core_predict_model_set <- function(train, future_dates, metric_row, config_row,
                                   ensemble_weights = NULL) {
  unit <- metric_row$unit[1L]
  comparison_months <- core_config_integer(
    config_row, "drift_comparison_months", 1L
  )
  drift_damping <- core_config_number(config_row, "drift_damping", 0, 1)
  trend_damping <- core_config_number(config_row, "trend_damping", 0, 1)
  trend_window <- core_config_integer(config_row, "recent_trend_months", 18L)
  seasonal_change_years <- core_config_integer(
    config_row, "seasonal_change_years", 1L
  )
  minimum_components <- core_config_integer(
    config_row, "minimum_ensemble_components", 1L, 3L
  )
  safe <- function(expression) suppressWarnings(tryCatch(
    expression,
    error = function(e) rep(NA_real_, length(future_dates))
  ))
  seasonal_naive <- safe(core_predict_seasonal_naive(train, future_dates))
  recent_level_seasonal <- safe(core_predict_recent_level_seasonal(
    train, future_dates, unit, seasonal_change_years
  ))
  seasonal_drift <- safe(core_predict_seasonal_drift(
    train, future_dates, unit, comparison_months, drift_damping
  ))
  recent_trend <- safe(core_predict_recent_trend(
    train, future_dates, unit, trend_window, trend_damping
  ))
  components <- cbind(
    recent_level_seasonal, seasonal_drift, recent_trend
  )
  reference <- core_weighted_component_mean(
    components, ensemble_weights, minimum_components
  )
  data.table::rbindlist(list(
    data.table::data.table(model = "seasonal_naive", predicted_value = seasonal_naive),
    data.table::data.table(
      model = "recent_level_seasonal",
      predicted_value = recent_level_seasonal
    ),
    data.table::data.table(model = "seasonal_drift_damped", predicted_value = seasonal_drift),
    data.table::data.table(model = "recent_trend_seasonal", predicted_value = recent_trend),
    data.table::data.table(model = "reference_ensemble", predicted_value = reference)
  ))[, `:=`(
    forecast_month = rep(data.table::as.IDate(future_dates), times = 5L),
    horizon_months = rep(seq_along(future_dates), times = 5L)
  )]
}

core_rebuild_rolling_reference <- function(predictions, config_row) {
  components <- data.table::copy(predictions[
    model %in% core_ensemble_component_models()
  ])
  non_reference <- data.table::copy(predictions[model != "reference_ensemble"])
  minimum_components <- core_config_integer(
    config_row, "minimum_ensemble_components", 1L, 3L
  )
  target_months <- sort(unique(components$target_month))
  references <- lapply(target_months, function(target) {
    weights <- core_estimate_ensemble_weights(
      components, config_row, before_target = target
    )
    current <- merge(
      components[target_month == target],
      weights[, .(model, ensemble_weight, weighting_method)],
      by = "model", all.x = TRUE
    )
    current[, {
      available <- is.finite(predicted_value) &
        is.finite(ensemble_weight) & ensemble_weight > 0
      prediction <- if (sum(available) >= minimum_components) {
        stats::weighted.mean(
          predicted_value[available], ensemble_weight[available]
        )
      } else {
        NA_real_
      }
      list(
        model = "reference_ensemble",
        predicted_value = prediction,
        weighting_method = weighting_method[1L]
      )
    }, by = .(
      metric_id, entity_id, entity_name, origin_month, target_month,
      actual_value, actual_denominator
    )]
  })
  reference <- data.table::rbindlist(references, use.names = TRUE, fill = TRUE)
  reference <- reference[is.finite(predicted_value)]
  reference[, `:=`(
    error_native = actual_value - predicted_value,
    absolute_error_native = abs(actual_value - predicted_value),
    squared_error_native = (actual_value - predicted_value)^2
  )]
  out <- data.table::rbindlist(
    list(non_reference, reference), use.names = TRUE, fill = TRUE
  )
  data.table::setorder(out, entity_id, target_month, model)
  out[]
}

core_rolling_one_step <- function(panel, metric_row, config_row,
                                  allow_empty = FALSE) {
  minimum_training <- core_config_integer(
    config_row, "minimum_training_months", 12L
  )
  backtest_months <- core_config_integer(config_row, "backtest_months", 6L)
  pieces <- list()
  piece_index <- 0L
  entities <- unique(panel$entity_id)
  for (entity in entities) {
    z <- data.table::copy(panel[entity_id == entity])
    data.table::setorder(z, calendar_month)
    target_start <- max(min(z$calendar_month), data.table::as.IDate(seq(
      as.Date(max(z$calendar_month)), by = "-1 month", length.out = backtest_months
    )[backtest_months]))
    targets <- z[
      calendar_month >= target_start & complete_submission == TRUE & is.finite(value)
    ]
    for (i in seq_len(nrow(targets))) {
      target_month <- targets$calendar_month[i]
      origin_month <- data.table::as.IDate(seq(
        as.Date(target_month), by = "-1 month", length.out = 2L
      )[2L])
      train <- core_contiguous_tail(z[calendar_month <= origin_month])
      if (nrow(train) < minimum_training ||
          max(train$calendar_month) != origin_month) next
      predicted <- core_predict_model_set(
        train, target_month, metric_row, config_row
      )
      predicted <- predicted[is.finite(predicted_value)]
      if (!nrow(predicted)) next
      piece_index <- piece_index + 1L
      predicted[, `:=`(
        metric_id = metric_row$metric_id[1L],
        entity_id = entity,
        entity_name = targets$entity_name[i],
        origin_month = origin_month,
        target_month = target_month,
        actual_value = targets$value[i],
        actual_denominator = targets$denominator[i]
      )]
      predicted[, c("forecast_month", "horizon_months") := NULL]
      pieces[[piece_index]] <- predicted
    }
  }
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(out)) {
    if (allow_empty) return(data.table::data.table())
    stop("No rolling one-step predictions for ", metric_row$metric_id[1L], ".")
  }
  out[, `:=`(
    error_native = actual_value - predicted_value,
    absolute_error_native = abs(actual_value - predicted_value),
    squared_error_native = (actual_value - predicted_value)^2
  )]
  core_rebuild_rolling_reference(out, config_row)
}

core_score_predictions <- function(rolling) {
  rolling[, .(
    n_predictions = .N,
    n_entities = data.table::uniqueN(entity_id),
    mae_native = mean(absolute_error_native),
    rmse_native = sqrt(mean(squared_error_native)),
    bias_native = mean(error_native)
  ), by = model][order(rmse_native)][, rmse_rank := seq_len(.N)][]
}

core_forecast_method_record <- function(metric_row, config_row, national_panel,
                                        rolling, scores, all_forecasts,
                                        next_release, ensemble_weights) {
  reference_score <- scores[model == "reference_ensemble"]
  naive_score <- scores[model == "seasonal_naive"]
  reference_backtest <- rolling[model == "reference_ensemble"]
  next_components <- all_forecasts[
    horizon_months == 1L & model %in% core_ensemble_component_models() &
      is.finite(predicted_value), model
  ]
  unit <- metric_row$unit[1L]
  transform_label <- if (unit == "proportion") {
    "logit scale; converted back to a bounded proportion"
  } else if (unit %in% c("minutes", "count")) {
    "log scale; converted back to a non-negative level"
  } else {
    "original scale"
  }
  data.table::data.table(
    metric_id = metric_row$metric_id[1L],
    display_name = metric_row$display_name[1L],
    forecast_method_id = "backtest_weighted_current_level_ensemble_v2",
    forecast_method_label = paste(
      "Backtest-weighted ensemble of recent-level seasonality, damped annual",
      "drift and a damped recent seasonal trend"
    ),
    model_selection_rule = paste(
      "Common three-component ensemble; inverse-RMSE weights are estimated",
      "from prior rolling forecasts and shrunk toward equal weights"
    ),
    modelling_scale = transform_label,
    seasonal_persistence_definition =
      paste(
        "Benchmark only: uses the latest observed value for the same calendar",
        "month; it is not an ensemble component."
      ),
    recent_level_seasonal_definition = paste0(
      "Starts from the latest observed value and adds centred median monthly ",
      "seasonal changes estimated from the latest ",
      core_config_integer(config_row, "seasonal_change_years", 1L),
      " years."
    ),
    annual_drift_definition = paste0(
      "Adds the mean year-on-year change from the latest ",
      core_config_integer(config_row, "drift_comparison_months", 1L),
      " comparisons; each additional forecast year is damped by ",
      format(core_config_number(config_row, "drift_damping", 0, 1), trim = TRUE),
      "."
    ),
    recent_trend_definition = paste0(
      "Linear trend plus calendar-month effects fitted to the latest ",
      core_config_integer(config_row, "recent_trend_months", 18L),
      " months; the forward trend is damped by ",
      format(core_config_number(config_row, "trend_damping", 0, 1), trim = TRUE),
      "."
    ),
    ensemble_rule = paste0(
      "Inverse-RMSE weights from rolling one-step forecasts, shrunk ",
      format(
        100 * core_config_number(
          config_row, "ensemble_equal_weight_shrinkage", 0, 1
        ), trim = TRUE
      ),
      "% toward equal weights; at least ",
      core_config_integer(config_row, "minimum_ensemble_components", 1L, 3L),
      " of 3 required."
    ),
    ensemble_weight_summary = paste0(
      ensemble_weights$model, "=",
      format(round(ensemble_weights$ensemble_weight, 4L), nsmall = 4L),
      collapse = ";"
    ),
    ensemble_weighting_target_months = ensemble_weights$weighting_target_months[1L],
    ensemble_weighting_method = ensemble_weights$weighting_method[1L],
    minimum_ensemble_components = core_config_integer(
      config_row, "minimum_ensemble_components", 1L, 3L
    ),
    components_available_next_release = length(next_components),
    component_names_next_release = paste(next_components, collapse = ";"),
    minimum_training_months = core_config_integer(
      config_row, "minimum_training_months", 12L
    ),
    final_fit_consecutive_months = next_release$training_months_n[1L],
    data_first_month = min(national_panel$calendar_month),
    data_through_month = next_release$data_through_month[1L],
    forecast_month = next_release$forecast_month[1L],
    configured_backtest_months = core_config_integer(
      config_row, "backtest_months", 6L
    ),
    backtest_predictions_scored = nrow(reference_backtest),
    backtest_first_target_month = if (nrow(reference_backtest)) {
      min(reference_backtest$target_month)
    } else {
      data.table::as.IDate(NA_character_)
    },
    backtest_last_target_month = if (nrow(reference_backtest)) {
      max(reference_backtest$target_month)
    } else {
      data.table::as.IDate(NA_character_)
    },
    reference_mae_native = if (nrow(reference_score)) {
      reference_score$mae_native[1L]
    } else {
      NA_real_
    },
    reference_rmse_native = if (nrow(reference_score)) {
      reference_score$rmse_native[1L]
    } else {
      NA_real_
    },
    reference_bias_native = if (nrow(reference_score)) {
      reference_score$bias_native[1L]
    } else {
      NA_real_
    },
    seasonal_naive_rmse_native = if (nrow(naive_score)) {
      naive_score$rmse_native[1L]
    } else {
      NA_real_
    },
    interval_method = next_release$interval_method[1L],
    interval_calibration_n = next_release$interval_calibration_n[1L],
    interval_calibration_window_months =
      next_release$interval_calibration_window_months[1L],
    interval_residual_center_native =
      next_release$interval_residual_center_native[1L],
    interval_centering_method = next_release$interval_centering_method[1L],
    interval_definition = if (
      core_config_character(
        config_row, "interval_calibration_method",
        c("empirical_error_quantiles", "recent_centered_symmetric_absolute_error")
      ) == "recent_centered_symmetric_absolute_error"
    ) {
      paste(
        "Recent one-step reference-model errors are median-centred and their",
        "absolute magnitudes set a symmetric predictive range around the point",
        "forecast; logical bounds are applied symmetrically and longer horizons",
        "scale widths by the square root of horizon."
      )
    } else {
      paste(
        "Historic one-step reference-model errors form the predictive range;",
        "longer horizons scale those errors by the square root of horizon."
      )
    },
    point_forecast = next_release$predicted_value[1L],
    lower_80 = next_release$lower_80[1L],
    upper_80 = next_release$upper_80[1L]
  )
}

core_make_final_forecasts <- function(panel, metric_row, config_row,
                                      ensemble_weights = NULL,
                                      allow_empty = FALSE) {
  latest_month <- max(panel$calendar_month)
  end_month <- data.table::as.IDate(config_row$forecast_end_month[1L])
  future_dates <- future_month_sequence(latest_month, end_month)
  minimum_training <- core_config_integer(
    config_row, "minimum_training_months", 12L
  )
  current <- panel[
    calendar_month == latest_month & complete_submission == TRUE & is.finite(value)
  ]
  pieces <- vector("list", nrow(current))
  for (i in seq_len(nrow(current))) {
    train <- core_contiguous_tail(panel[
      entity_id == current$entity_id[i] & calendar_month <= latest_month
    ])
    if (nrow(train) < minimum_training ||
        max(train$calendar_month) != latest_month) next
    forecast <- core_predict_model_set(
      train, future_dates, metric_row, config_row, ensemble_weights
    )
    forecast[, `:=`(
      metric_id = metric_row$metric_id[1L],
      entity_id = current$entity_id[i],
      entity_name = current$entity_name[i],
      data_through_month = latest_month,
      latest_actual_value = current$value[i],
      latest_denominator = current$denominator[i],
      training_months_n = nrow(train)
    )]
    pieces[[i]] <- forecast
  }
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(out)) {
    if (allow_empty) return(data.table::data.table())
    stop("No final forecasts for ", metric_row$metric_id[1L], ".")
  }
  data.table::setorder(out, entity_id, model, forecast_month)
  out[]
}

core_interval_calibration <- function(residuals, empirical_minimum_n,
                                      parametric_minimum_n, label,
                                      calibration_method =
                                        "empirical_error_quantiles") {
  z <- as.numeric(residuals[is.finite(residuals)])
  calibration_n <- length(z)
  probabilities <- c(0.025, 0.10, 0.90, 0.975)
  symmetric <- identical(
    calibration_method, "recent_centered_symmetric_absolute_error"
  )
  residual_center <- if (symmetric && calibration_n) stats::median(z) else 0
  if (calibration_n >= empirical_minimum_n) {
    if (symmetric) {
      radii <- stats::quantile(
        abs(z - residual_center), c(0.80, 0.95), names = FALSE, type = 8
      )
      return(list(
        quantiles = c(-radii[2L], -radii[1L], radii[1L], radii[2L]),
        calibration_n = calibration_n,
        method = "centered_symmetric_absolute_error_empirical",
        residual_center = residual_center,
        centering_method = "median_removed_before_absolute_error_calibration"
      ))
    }
    return(list(
      quantiles = stats::quantile(
        z, probabilities, names = FALSE, type = 8
      ),
      calibration_n = calibration_n,
      method = "empirical",
      residual_center = residual_center,
      centering_method = "none"
    ))
  }
  if (calibration_n < parametric_minimum_n) {
    stop(
      "Too few reference-model residuals to calibrate an interval for ",
      label, ": ", calibration_n, " available; ", parametric_minimum_n,
      " required for the small-sample fallback."
    )
  }
  residual_scale <- stats::sd(z)
  if (!is.finite(residual_scale) || residual_scale <= 0) {
    stop(
      "Cannot calibrate a small-sample interval for ", label,
      ": residual standard deviation is not positive."
    )
  }
  predictive_scale <- residual_scale * sqrt(1 + 1 / calibration_n)
  if (symmetric) {
    radii <- stats::qt(c(0.90, 0.975), df = calibration_n - 1L) *
      predictive_scale
    return(list(
      quantiles = c(-radii[2L], -radii[1L], radii[1L], radii[2L]),
      calibration_n = calibration_n,
      method = "centered_symmetric_student_t_predictive_small_sample",
      residual_center = residual_center,
      centering_method = "median_removed_before_symmetric_student_t_calibration"
    ))
  }
  list(
    quantiles = mean(z) + stats::qt(
      probabilities, df = calibration_n - 1L
    ) * predictive_scale,
    calibration_n = calibration_n,
    method = "student_t_predictive_small_sample",
    residual_center = residual_center,
    centering_method = "none"
  )
}

core_add_intervals <- function(forecast, rolling, metric_row, config_row,
                               provider_pool = FALSE) {
  reference_residuals <- rolling[model == "reference_ensemble"]
  calibration_months <- core_config_integer(
    config_row, "interval_calibration_months", 1L
  )
  calibration_method <- core_config_character(
    config_row, "interval_calibration_method",
    c("empirical_error_quantiles", "recent_centered_symmetric_absolute_error")
  )
  if (nrow(reference_residuals)) {
    latest_target_id <- max(month_id(reference_residuals$target_month))
    reference_residuals <- reference_residuals[
      month_id(target_month) >= latest_target_id - calibration_months + 1L
    ]
  }
  minimum_n <- core_config_integer(
    config_row, "minimum_interval_residuals", 5L
  )
  parametric_minimum_n <- core_config_integer(
    config_row, "minimum_parametric_interval_residuals", 5L, minimum_n
  )
  metric_id_value <- metric_row$metric_id[1L]
  pool_label <- if (provider_pool) {
    paste0(metric_id_value, " pooled provider forecasts")
  } else {
    paste0(metric_id_value, " national forecast")
  }
  pooled <- core_interval_calibration(
    reference_residuals$error_native,
    empirical_minimum_n = minimum_n,
    parametric_minimum_n = parametric_minimum_n,
    label = pool_label,
    calibration_method = calibration_method
  )
  if (grepl("student_t_predictive_small_sample", pooled$method, fixed = TRUE)) {
    message(
      metric_id_value, ": using a small-sample Student-t predictive interval from ",
      pooled$calibration_n, " one-step residuals (empirical threshold ",
      minimum_n, ")."
    )
  }
  out <- data.table::copy(forecast)
  interval_rows <- lapply(seq_len(nrow(out)), function(i) {
    calibration <- reference_residuals[entity_id == out$entity_id[i], error_native]
    use_entity <- length(calibration[is.finite(calibration)]) >= minimum_n
    selected <- if (use_entity) {
      core_interval_calibration(
        calibration,
        empirical_minimum_n = minimum_n,
        parametric_minimum_n = minimum_n,
        label = paste(metric_id_value, out$entity_id[i]),
        calibration_method = calibration_method
      )
    } else {
      pooled
    }
    quantiles <- selected$quantiles
    scale <- sqrt(out$horizon_months[i])
    point <- out$predicted_value[i]
    if (calibration_method == "recent_centered_symmetric_absolute_error") {
      radius_80 <- abs(quantiles[3L]) * scale
      radius_95 <- abs(quantiles[4L]) * scale
      if (metric_row$unit[1L] == "proportion") {
        logical_bound <- max(0, min(point, 1 - point))
        radius_80 <- min(radius_80, logical_bound)
        radius_95 <- min(radius_95, logical_bound)
      } else if (metric_row$unit[1L] %in% c("minutes", "count")) {
        radius_80 <- min(radius_80, point)
        radius_95 <- min(radius_95, point)
      }
      raw_lower_95 <- point - radius_95
      raw_lower_80 <- point - radius_80
      raw_upper_80 <- point + radius_80
      raw_upper_95 <- point + radius_95
    } else {
      raw_lower_95 <- core_bound_value(point + quantiles[1L] * scale,
                                       metric_row$unit[1L])
      raw_lower_80 <- core_bound_value(point + quantiles[2L] * scale,
                                       metric_row$unit[1L])
      raw_upper_80 <- core_bound_value(point + quantiles[3L] * scale,
                                       metric_row$unit[1L])
      raw_upper_95 <- core_bound_value(point + quantiles[4L] * scale,
                                       metric_row$unit[1L])
    }
    data.table::data.table(
      lower_95 = min(point, raw_lower_95),
      lower_80 = min(point, raw_lower_80),
      upper_80 = max(point, raw_upper_80),
      upper_95 = max(point, raw_upper_95),
      interval_calibration_n = selected$calibration_n,
      interval_calibration_window_months = calibration_months,
      interval_residual_center_native = selected$residual_center,
      interval_centering_method = selected$centering_method,
      interval_method = if (use_entity) {
        paste0(
          "entity_", selected$method,
          "_one_step_scaled_by_sqrt_horizon"
        )
      } else if (provider_pool) {
        paste0(
          "pooled_provider_", selected$method,
          "_one_step_scaled_by_sqrt_horizon"
        )
      } else {
        paste0(
          "national_", selected$method,
          "_one_step_scaled_by_sqrt_horizon"
        )
      }
    )
  })
  cbind(out, data.table::rbindlist(interval_rows))[]
}

core_forecast_reversal_diagnostic <- function(panel, next_release, metric_row,
                                              config_row) {
  window <- core_config_integer(
    config_row, "forecast_reversal_window_months", 2L
  )
  threshold <- core_config_number(
    config_row, "forecast_reversal_threshold_native", 0
  )
  z <- core_contiguous_tail(panel)
  data.table::setorder(z, calendar_month)
  z <- utils::tail(z, window + 1L)
  changes <- if (nrow(z) >= 2L) diff(z$value) else numeric()
  flat_threshold <- as.numeric(metric_row$flat_threshold_native[1L])
  recent_direction <- if (
    length(changes) == window && all(changes > flat_threshold)
  ) {
    "rising"
  } else if (
    length(changes) == window && all(changes < -flat_threshold)
  ) {
    "falling"
  } else {
    "mixed_or_flat"
  }
  latest_value <- if (nrow(z)) utils::tail(z$value, 1L) else NA_real_
  forecast_change <- next_release$predicted_value[1L] - latest_value
  reversal <- is.finite(forecast_change) &&
    abs(forecast_change) >= threshold &&
    (
      (recent_direction == "falling" && forecast_change > 0) ||
        (recent_direction == "rising" && forecast_change < 0)
    )
  reason <- if (reversal) {
    paste0(
      "Forecast change of ", signif(forecast_change, 4L),
      " reverses ", window, " consecutive ", recent_direction,
      " movements and exceeds the review threshold of ",
      signif(threshold, 4L), "."
    )
  } else {
    "No material reversal of a sustained recent movement."
  }
  data.table::data.table(
    metric_id = metric_row$metric_id[1L],
    data_through_month = next_release$data_through_month[1L],
    forecast_month = next_release$forecast_month[1L],
    recent_direction_months = as.integer(window),
    recent_actual_direction = recent_direction,
    recent_change_native = if (length(changes) == window) sum(changes) else NA_real_,
    forecast_change_native = forecast_change,
    reversal_threshold_native = threshold,
    forecast_reversal_flag = reversal,
    forecast_reversal_reason = reason
  )
}

core_archive_rows <- function(next_release, metric_row, config_row) {
  next_release[, .(
    metric_id,
    forecast_version = config_row$release_forecast_version[1L],
    data_through_month,
    forecast_month,
    entity_id,
    entity_name,
    model,
    predicted_value,
    lower_80,
    upper_80,
    lower_95,
    upper_95,
    interval_calibration_n,
    interval_method,
    interval_calibration_window_months,
    interval_residual_center_native,
    interval_centering_method,
    latest_actual_value,
    unit = metric_row$unit[1L],
    higher_is_better = metric_row$higher_is_better[1L]
  )]
}

core_score_release_archive <- function(archive, panel) {
  forecast <- data.table::copy(archive)
  forecast[, forecast_month_join___ := as.character(forecast_month)]
  actual <- panel[, .(
    metric_id,
    entity_id,
    forecast_month_join___ = as.character(calendar_month),
    actual_value = data.table::fifelse(complete_submission, value, NA_real_),
    actual_denominator = data.table::fifelse(
      complete_submission, denominator, NA_real_
    )
  )]
  out <- merge(
    forecast, actual,
    by = c("metric_id", "entity_id", "forecast_month_join___"),
    all.x = TRUE
  )
  out[, `:=`(
    forecast_status = data.table::fifelse(
      is.finite(actual_value), "scored", "awaiting_release"
    ),
    error_native = actual_value - predicted_value
  )]
  out[, forecast_month_join___ := NULL]
  out[]
}

core_surprise_history <- function(rolling, scorecard, config_row) {
  simulated <- rolling[model == "reference_ensemble", .(
    metric_id,
    entity_id,
    entity_name,
    target_month,
    origin_month,
    predicted_value,
    actual_value,
    error_native,
    forecast_evidence = "historically_simulated"
  )]
  genuine <- scorecard[
    forecast_version == config_row$release_forecast_version[1L] &
      model == "reference_ensemble" & forecast_status == "scored",
    .(
      metric_id,
      entity_id,
      entity_name,
      target_month = data.table::as.IDate(forecast_month),
      origin_month = data.table::as.IDate(data_through_month),
      predicted_value,
      actual_value,
      error_native,
      forecast_evidence = "genuine_release_vintage"
    )
  ]
  combined <- data.table::rbindlist(
    list(genuine, simulated), use.names = TRUE, fill = TRUE
  )
  combined[, evidence_priority___ := data.table::fifelse(
    forecast_evidence == "genuine_release_vintage", 1L, 2L
  )]
  data.table::setorder(combined, entity_id, target_month, evidence_priority___)
  combined <- unique(combined, by = c("metric_id", "entity_id", "target_month"))
  combined[, evidence_priority___ := NULL]
  data.table::setorder(combined, entity_id, target_month)
  combined[]
}

core_provider_signals <- function(surprises, provider_panel, metric_row, config_row) {
  window <- core_config_integer(config_row, "signal_window_months", 2L)
  threshold <- core_config_number(config_row, "signal_materiality_native", 0)
  direction_share_required <- core_config_number(
    config_row, "signal_direction_share", 0.5, 1
  )
  minimum_denominator <- core_config_number(
    config_row, "minimum_provider_denominator", 0
  )
  higher_is_better <- isTRUE(metric_row$higher_is_better[1L])
  latest_month <- max(provider_panel$calendar_month)
  entities <- unique(provider_panel$entity_id)
  rows <- lapply(entities, function(entity) {
    history <- data.table::copy(surprises[entity_id == entity])
    data.table::setorder(history, target_month)
    history <- utils::tail(history[target_month <= latest_month], window)
    provider_window <- data.table::copy(provider_panel[
      entity_id == entity & calendar_month <= latest_month
    ])
    data.table::setorder(provider_window, calendar_month)
    provider_window <- utils::tail(provider_window, window)
    use_denominator <- nrow(provider_window) == window &&
      all(is.finite(provider_window$denominator))
    use_activity_proxy <- !use_denominator &&
      "activity_volume_proxy" %in% names(provider_window) &&
      nrow(provider_window) == window &&
      all(is.finite(provider_window$activity_volume_proxy))
    volume <- if (use_denominator) {
      provider_window$denominator
    } else if (use_activity_proxy) {
      provider_window$activity_volume_proxy
    } else {
      rep(NA_real_, window)
    }
    volume_measure <- if (use_denominator) {
      "published_metric_denominator"
    } else if (use_activity_proxy) {
      "two_hour_referrals_received_activity_proxy"
    } else {
      "unavailable"
    }
    minimum_volume <- if (length(volume) && all(is.finite(volume))) {
      min(volume)
    } else {
      NA_real_
    }
    volume_ok <- !is.finite(minimum_denominator) || minimum_denominator <= 0 ||
      (is.finite(minimum_volume) && minimum_volume >= minimum_denominator)
    consecutive <- nrow(history) == window &&
      all(diff(month_id(history$target_month)) == 1L) &&
      max(history$target_month) == latest_month
    eligibility_reason <- if (!consecutive) {
      "insufficient_consecutive_forecast_surprises"
    } else if (!length(volume) || any(!is.finite(volume))) {
      "missing_volume_for_signal_window"
    } else if (!volume_ok) {
      "below_minimum_volume"
    } else {
      "eligible"
    }
    if (!consecutive || !volume_ok) {
      return(data.table::data.table(
        metric_id = metric_row$metric_id[1L],
        entity_id = entity,
        entity_name = core_latest_nonmissing_character(
          provider_panel[entity_id == entity, entity_name]
        ),
        data_through_month = latest_month,
        signal_months_n = nrow(history),
        mean_error_native = NA_real_,
        favourable_gap_native = NA_real_,
        direction_share = NA_real_,
        latest_error_same_direction = NA,
        signal = "insufficient_history_or_volume",
        signal_evidence = NA_character_,
        signal_eligibility_reason = eligibility_reason,
        volume_measure = volume_measure,
        minimum_volume_in_signal_window = minimum_volume,
        minimum_required_volume = minimum_denominator
      ))
    }
    favourable_errors <- if (higher_is_better) {
      history$error_native
    } else {
      -history$error_native
    }
    mean_error <- mean(history$error_native)
    favourable_gap <- mean(favourable_errors)
    expected_sign <- sign(favourable_gap)
    direction_share <- mean(sign(favourable_errors) == expected_sign)
    latest_same <- sign(utils::tail(favourable_errors, 1L)) == expected_sign
    signal <- if (
      favourable_gap >= threshold &&
        direction_share >= direction_share_required && latest_same
    ) {
      "sustained_favourable"
    } else if (
      favourable_gap <= -threshold &&
        direction_share >= direction_share_required && latest_same
    ) {
      "sustained_adverse"
    } else {
      "no_sustained_signal"
    }
    evidence <- if (all(
      history$forecast_evidence == "genuine_release_vintage"
    )) {
      "genuine_release_vintages"
    } else if (any(
      history$forecast_evidence == "genuine_release_vintage"
    )) {
      "mixed_genuine_and_simulated"
    } else {
      "historically_simulated"
    }
    data.table::data.table(
      metric_id = metric_row$metric_id[1L],
      entity_id = entity,
      entity_name = core_latest_nonmissing_character(history$entity_name),
      data_through_month = latest_month,
      signal_months_n = window,
      mean_error_native = mean_error,
      favourable_gap_native = favourable_gap,
      direction_share = direction_share,
      latest_error_same_direction = latest_same,
      signal = signal,
      signal_evidence = evidence,
      signal_eligibility_reason = "eligible",
      volume_measure = volume_measure,
      minimum_volume_in_signal_window = minimum_volume,
      minimum_required_volume = minimum_denominator
    )
  })
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  data.table::setorder(out, -favourable_gap_native)
  out[]
}

core_provider_watchlist <- function(signals, provider_panel, provider_next) {
  latest_month <- max(provider_panel$calendar_month)
  latest <- provider_panel[
    calendar_month == latest_month,
    .(metric_id, entity_id, latest_value = value,
      latest_denominator = denominator,
      latest_activity_volume_proxy = activity_volume_proxy,
      activity_volume_proxy_method, complete_submission)
  ]
  next_values <- provider_next[, .(
    metric_id, entity_id, forecast_month,
    next_release_forecast = predicted_value,
    lower_80, upper_80
  )]
  out <- merge(signals, latest, by = c("metric_id", "entity_id"), all.x = TRUE)
  out <- merge(out, next_values, by = c("metric_id", "entity_id"), all.x = TRUE)
  out[, spotlight_rank := data.table::frank(
    -abs(favourable_gap_native), ties.method = "first", na.last = "keep"
  )]
  data.table::setorder(out, signal, spotlight_rank)
  out[]
}

core_empty_provider_watchlist <- function() {
  data.table::data.table(
    metric_id = character(),
    entity_id = character(),
    entity_name = character(),
    data_through_month = data.table::as.IDate(character()),
    signal_months_n = integer(),
    mean_error_native = numeric(),
    favourable_gap_native = numeric(),
    direction_share = numeric(),
    latest_error_same_direction = logical(),
    signal = character(),
    signal_evidence = character(),
    signal_eligibility_reason = character(),
    volume_measure = character(),
    minimum_volume_in_signal_window = numeric(),
    minimum_required_volume = numeric(),
    latest_value = numeric(),
    latest_denominator = numeric(),
    latest_activity_volume_proxy = numeric(),
    activity_volume_proxy_method = character(),
    complete_submission = logical(),
    forecast_month = data.table::as.IDate(character()),
    next_release_forecast = numeric(),
    lower_80 = numeric(),
    upper_80 = numeric(),
    spotlight_rank = numeric()
  )
}

core_target_for_overview <- function(targets, latest_month) {
  z <- data.table::copy(targets)
  z[, target_month := data.table::as.IDate(target_month)]
  future <- z[target_month > latest_month][order(target_order)]
  if (nrow(future)) future[1L] else z[order(-target_order)][1L]
}

core_overview_row <- function(metric_row, national_panel, national_projection,
                              national_next, national_archive,
                              national_scorecard, provider_watchlist, targets) {
  latest_month <- max(national_panel$calendar_month)
  latest <- national_panel[calendar_month == latest_month]
  if (nrow(latest) != 1L || nrow(national_next) != 1L) {
    stop("Core overview expects one latest actual and one next forecast for ",
         metric_row$metric_id[1L], ".")
  }
  target <- core_target_for_overview(targets, latest_month)
  target_projection <- national_projection[
    forecast_month == target$target_month[1L], predicted_value
  ]
  if (length(target_projection) != 1L || !is.finite(target_projection)) {
    stop("No national projection at the selected target month for ",
         metric_row$metric_id[1L], ".")
  }
  scored <- national_scorecard[
    forecast_status == "scored" & entity_id == "ENGLAND" &
      data.table::as.IDate(forecast_month) == latest_month
  ]
  if (nrow(scored) && "forecast_created_at_utc" %in% names(scored)) {
    data.table::setorder(scored, forecast_created_at_utc)
  }
  current_archive <- national_archive[
    entity_id == "ENGLAND" &
      data.table::as.IDate(data_through_month) == latest_month &
      data.table::as.IDate(forecast_month) == national_next$forecast_month[1L]
  ]
  if (nrow(current_archive) && "forecast_created_at_utc" %in% names(current_archive)) {
    data.table::setorder(current_archive, forecast_created_at_utc)
  }
  higher <- isTRUE(metric_row$higher_is_better[1L])
  change <- national_next$predicted_value[1L] - latest$value[1L]
  favourable_change <- if (higher) change else -change
  expected_direction <- if (abs(favourable_change) < metric_row$flat_threshold_native[1L]) {
    "broadly_stable"
  } else if (favourable_change > 0) {
    "improving"
  } else {
    "deteriorating"
  }
  target_gap <- target_projection - target$target_value[1L]
  favourable_target_gap <- if (higher) target_gap else -target_gap
  assessed <- sum(is.finite(provider_watchlist$favourable_gap_native))
  favourable_n <- sum(provider_watchlist$signal == "sustained_favourable", na.rm = TRUE)
  adverse_n <- sum(provider_watchlist$signal == "sustained_adverse", na.rm = TRUE)
  data.table::data.table(
    metric_id = metric_row$metric_id[1L],
    display_order = as.integer(metric_row$display_order[1L]),
    display_name = metric_row$display_name[1L],
    short_name = metric_row$short_name[1L],
    unit = metric_row$unit[1L],
    digits = as.integer(metric_row$digits[1L]),
    higher_is_better = higher,
    flat_threshold_native = metric_row$flat_threshold_native[1L],
    deep_dive_file = metric_row$deep_dive_file[1L],
    latest_month = latest_month,
    latest_value = latest$value[1L],
    latest_vintage_forecast_value = if (nrow(scored)) {
      scored$predicted_value[nrow(scored)]
    } else {
      NA_real_
    },
    latest_vintage_forecast_error = if (nrow(scored)) {
      latest$value[1L] - scored$predicted_value[nrow(scored)]
    } else {
      NA_real_
    },
    latest_vintage_lower_80 = if (
      nrow(scored) && "lower_80" %in% names(scored)
    ) scored$lower_80[nrow(scored)] else NA_real_,
    latest_vintage_upper_80 = if (
      nrow(scored) && "upper_80" %in% names(scored)
    ) scored$upper_80[nrow(scored)] else NA_real_,
    forecast_month = national_next$forecast_month[1L],
    forecast_value = national_next$predicted_value[1L],
    lower_80 = national_next$lower_80[1L],
    upper_80 = national_next$upper_80[1L],
    forecast_change_native = change,
    expected_direction = expected_direction,
    recent_actual_direction = if (
      "recent_actual_direction" %in% names(national_next)
    ) national_next$recent_actual_direction[1L] else NA_character_,
    recent_direction_months = if (
      "recent_direction_months" %in% names(national_next)
    ) national_next$recent_direction_months[1L] else NA_integer_,
    forecast_reversal_flag = if (
      "forecast_reversal_flag" %in% names(national_next)
    ) national_next$forecast_reversal_flag[1L] else FALSE,
    forecast_reversal_reason = if (
      "forecast_reversal_reason" %in% names(national_next)
    ) national_next$forecast_reversal_reason[1L] else NA_character_,
    target_month = target$target_month[1L],
    target_value = target$target_value[1L],
    target_label = target$target_label[1L],
    projected_target_value = target_projection,
    target_gap_native = target_gap,
    trajectory_status = if (favourable_target_gap >= 0) {
      "on_trajectory"
    } else {
      "off_course"
    },
    providers_assessed = as.integer(assessed),
    providers_favourable = as.integer(favourable_n),
    providers_adverse = as.integer(adverse_n),
    providers_no_signal = as.integer(assessed - favourable_n - adverse_n),
    forecast_issued_at_utc = if (
      nrow(current_archive) && "forecast_created_at_utc" %in% names(current_archive)
    ) {
      current_archive$forecast_created_at_utc[nrow(current_archive)]
    } else {
      NA_character_
    }
  )
}
