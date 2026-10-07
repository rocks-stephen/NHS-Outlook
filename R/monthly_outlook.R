outlook_html_escape <- function(x) {
  z <- as.character(x)
  z <- gsub("&", "&amp;", z, fixed = TRUE)
  z <- gsub("<", "&lt;", z, fixed = TRUE)
  z <- gsub(">", "&gt;", z, fixed = TRUE)
  z <- gsub('"', "&quot;", z, fixed = TRUE)
  z
}

outlook_format_percent <- function(x, digits = 1L) {
  ifelse(is.na(x), "Not available", sprintf(paste0("%.", digits, "f%%"), 100 * x))
}

outlook_format_pp <- function(x, signed = FALSE, digits = 1L) {
  if (is.na(x)) return("Not available")
  format_string <- if (signed) paste0("%+.", digits, "fpp") else paste0("%.", digits, "fpp")
  sprintf(format_string, x)
}

outlook_format_month <- function(x) {
  format(as.Date(x), "%B %Y")
}

outlook_format_short_month <- function(x) {
  format(as.Date(x), "%b %y")
}

outlook_previous_month <- function(x) {
  data.table::as.IDate(seq(as.Date(x), by = "-1 month", length.out = 2L)[2L])
}

outlook_months_before <- function(x, months) {
  data.table::as.IDate(seq(
    as.Date(x), by = "-1 month", length.out = as.integer(months) + 1L
  )[as.integer(months) + 1L])
}

outlook_historical_month_change <- function(national, target_month) {
  required <- c("calendar_month", "ae4h_performance")
  assert_columns(national, required)
  z <- data.table::copy(national[is.finite(ae4h_performance)])
  if ("national_comparability_era" %in% names(z)) {
    z <- z[national_comparability_era %in% c(
      "monthly_full_pre_crs", "monthly_full_post_crs"
    )]
  } else if ("source_method" %in% names(z)) {
    z <- z[source_method == "monthly_collection"]
  }
  data.table::setorder(z, calendar_month)
  z[, `:=`(
    previous_month = data.table::shift(calendar_month),
    previous_performance = data.table::shift(ae4h_performance)
  )]
  z[, month_gap := month_id(calendar_month) - month_id(previous_month)]
  changes <- z[
    month_number(calendar_month) == month_number(target_month) &
      month_gap == 1L & !is.na(previous_performance),
    100 * (ae4h_performance - previous_performance)
  ]
  list(
    median_pp = if (length(changes)) stats::median(changes) else NA_real_,
    n = length(changes)
  )
}

outlook_forecast_driver_sentence <- function(
    forecast_row, forecast_components, materiality_pp = 0.2) {
  if (!nrow(forecast_components)) return("")
  required <- c("model", "forecast_month", "predicted_performance")
  if (!all(required %in% names(forecast_components))) return("")
  target_month <- data.table::as.IDate(forecast_row$forecast_month[1L])
  z <- data.table::copy(forecast_components)
  z[, forecast_month := data.table::as.IDate(forecast_month)]
  z <- z[forecast_month == target_month & is.finite(predicted_performance)]
  seasonal <- z[model == "seasonal_naive", predicted_performance]
  trend_bearing <- z[
    model %in% c("seasonal_drift_damped", "shared_season_current_trend"),
    predicted_performance
  ]
  if (length(seasonal) != 1L || length(trend_bearing) != 2L) return("")

  latest <- forecast_row$latest_actual_performance[1L]
  point <- forecast_row$predicted_performance[1L]
  if (!is.finite(latest) || !is.finite(point)) return("")
  seasonal_effect <- 100 * (seasonal[1L] - latest)
  trend_adjustment <- 100 * (mean(trend_bearing) - seasonal[1L])
  forecast_change <- 100 * (point - latest)
  classify <- function(x) {
    if (x >= materiality_pp) "favourable" else if (x <= -materiality_pp) "adverse" else "neutral"
  }
  direction <- classify(forecast_change)
  seasonal_direction <- classify(seasonal_effect)
  trend_direction <- classify(trend_adjustment)
  if (direction == "neutral") {
    return(paste0(
      " Seasonal and underlying-trend effects are small or offsetting, leaving ",
      "the outlook broadly stable."
    ))
  }

  improving <- direction == "favourable"
  direction_word <- if (improving) "improvement" else "deterioration"
  aligned <- if (improving) "favourable" else "adverse"
  opposed <- if (improving) "adverse" else "favourable"
  if (seasonal_direction == aligned && trend_direction == aligned) {
    return(paste0(
      " The model's expected ", direction_word,
      " reflects both a ",
      if (improving) "seasonally favourable" else "seasonally weaker",
      " month and an ", if (improving) "improving" else "deteriorating",
      " underlying trend."
    ))
  }
  if (seasonal_direction == aligned) {
    return(paste0(
      " The model's expected ", direction_word, " is mainly seasonal",
      if (trend_direction == opposed) {
        paste0(", partly offset by ",
          if (improving) "a weakening" else "an improving",
          " underlying trend."
        )
      } else {
        "; the underlying trend is broadly flat."
      }
    ))
  }
  if (trend_direction == aligned) {
    return(paste0(
      " The model's expected ", direction_word,
      " is mainly driven by the underlying trend",
      if (seasonal_direction == opposed) {
        paste0(", despite a ",
          if (improving) "seasonally weaker" else "seasonally favourable",
          " month."
        )
      } else {
        "; the seasonal effect is small."
      }
    ))
  }
  paste0(
    " The model components are mixed, but together imply a small ",
    direction_word, "."
  )
}

outlook_narrative <- function(
    national, medium_forecast, forecast_row,
    intermediate_target_month, intermediate_target_performance,
    forecast_components = data.table::data.table()) {
  point <- forecast_row$predicted_performance[1L]
  actual <- forecast_row$actual_performance[1L]
  target_month <- data.table::as.IDate(forecast_row$forecast_month[1L])
  data_through <- data.table::as.IDate(forecast_row$data_through_month[1L])
  latest <- forecast_row$latest_actual_performance[1L]
  target_projection <- medium_forecast[
    forecast_month == intermediate_target_month, predicted_performance
  ]
  target_sentence <- ""
  if (length(target_projection) && is.finite(target_projection[1L])) {
    target_gap <- 100 * (target_projection[1L] - intermediate_target_performance)
    target_sentence <- paste0(
      " The reference projection reaches ",
      outlook_format_percent(target_projection[1L]), " by ",
      outlook_format_month(intermediate_target_month), ", ",
      outlook_format_pp(abs(target_gap)), " ",
      if (target_gap >= 0) "above" else "below", " the ",
      outlook_format_percent(intermediate_target_performance, digits = 0L),
      " interim milestone."
    )
  }
  if (!is.na(actual)) {
    error <- 100 * (actual - point)
    direction <- if (error >= 0) "above" else "below"
    interval_text <- if (
      !is.na(forecast_row$lower_80[1L]) &&
        actual >= forecast_row$lower_80[1L] &&
        actual <= forecast_row$upper_80[1L]
    ) {
      "inside the expected 80% range"
    } else {
      "outside the expected 80% range"
    }
    return(paste0(
      "The published result was ", outlook_format_percent(actual), ", ",
      outlook_format_pp(abs(error)), " ", direction,
      " forecast and ", interval_text, ".", target_sentence
    ))
  }

  lookback_month <- outlook_months_before(data_through, 3L)
  lookback <- national[calendar_month == lookback_month, ae4h_performance]
  momentum_sentence <- if (length(lookback) && is.finite(lookback[1L])) {
    momentum <- 100 * (latest - lookback[1L])
    if (abs(momentum) < 0.5) {
      paste0(
        "National all-types performance was broadly flat over the three months to ",
        outlook_format_month(data_through), ", at ", outlook_format_percent(latest), "."
      )
    } else {
      paste0(
        "National all-types performance ",
        if (momentum > 0) "improved" else "fell", " by ",
        outlook_format_pp(abs(momentum)), " over the three months to ",
        outlook_format_month(data_through), ", reaching ",
        outlook_format_percent(latest), "."
      )
    }
  } else {
    paste0(
      "The latest national all-types result was ", outlook_format_percent(latest),
      " in ", outlook_format_month(data_through), "."
    )
  }

  forecast_change <- 100 * (point - latest)
  forecast_sentence <- if (abs(forecast_change) < 0.05) {
    paste0(
      " The ", outlook_format_month(target_month), " forecast is broadly unchanged at ",
      outlook_format_percent(point), "."
    )
  } else {
    paste0(
      " The ", outlook_format_month(target_month), " forecast is ",
      outlook_format_percent(point), ", ",
      outlook_format_pp(abs(forecast_change)), " ",
      if (forecast_change > 0) "above" else "below", " the latest result."
    )
  }
  driver_sentence <- outlook_forecast_driver_sentence(
    forecast_row, forecast_components
  )

  seasonal <- outlook_historical_month_change(national, target_month)
  seasonal_sentence <- ""
  if (!nzchar(driver_sentence) &&
      seasonal$n >= 3L && is.finite(seasonal$median_pp)) {
    seasonal_direction <- if (seasonal$median_pp <= -0.2) {
      "a small deterioration"
    } else if (seasonal$median_pp >= 0.2) {
      "a small improvement"
    } else {
      "little month-to-month change"
    }
    seasonal_sentence <- paste0(
      " Historically, ", format(as.Date(target_month), "%B"),
      " has typically brought ", seasonal_direction, " (median ",
      outlook_format_pp(seasonal$median_pp, signed = TRUE), " across ",
      seasonal$n, " comparable years)."
    )
  }
  paste0(
    momentum_sentence, forecast_sentence, driver_sentence,
    seasonal_sentence, target_sentence
  )
}

outlook_provider_commentary <- function(provider_assessed, above_n, below_n, within_n) {
  if (provider_assessed < 1L) {
    return("No provider has enough information for the sustained-deviation screen.")
  }
  balance <- if (below_n > above_n) {
    paste0(
      "Sustained adverse signals outnumber favourable signals ",
      "(", below_n, " versus ", above_n, ")."
    )
  } else if (above_n > below_n) {
    paste0(
      "Sustained favourable signals outnumber adverse signals ",
      "(", above_n, " versus ", below_n, ")."
    )
  } else {
    paste0(
      "Sustained favourable and adverse signals are balanced at ", above_n,
      " in each direction."
    )
  }
  paste0(
    balance, " Most providers (", within_n, " of ", provider_assessed,
    ") do not meet the active six-release threshold."
  )
}

outlook_provider_distribution_svg <- function(provider_signals,
                                               benchmark = 0.95) {
  z <- data.table::copy(provider_signals[
    is.finite(latest_actual_performance)
  ])
  if (!nrow(z)) {
    return(paste0(
      '<div class="distribution-empty">A provider distribution is not ',
      'available in this edition.</div>'
    ))
  }
  data.table::setorder(z, latest_actual_performance, analysis_trust_name)
  values <- c(z$latest_actual_performance, benchmark)
  padding <- max(diff(range(values)) * 0.08, 0.01)
  x_min <- max(0, min(values) - padding)
  x_max <- min(1, max(values) + padding)
  left <- 38; right <- 732; top <- 28; bottom <- 125
  x_position <- function(value) {
    left + (value - x_min) * (right - left) / max(1e-9, x_max - x_min)
  }
  z[, y___ := 51 + (seq_len(.N) %% 9L) * 7]
  z[, class___ := data.table::fcase(
    signal == "sustained_above_trajectory", "favourable",
    signal == "sustained_below_trajectory", "adverse",
    default = "neutral"
  )]
  dots <- paste(vapply(seq_len(nrow(z)), function(i) paste0(
    '<circle class="distribution-dot ', z$class___[i], '" cx="',
    sprintf("%.1f", x_position(z$latest_actual_performance[i])), '" cy="',
    z$y___[i], '" r="4"><title>',
    outlook_html_escape(z$analysis_trust_name[i]), ': ',
    outlook_format_percent(z$latest_actual_performance[i]),
    '</title></circle>'
  ), character(1)), collapse = "")
  benchmark_x <- x_position(benchmark)
  benchmark_anchor <- if (benchmark_x >= right - 130) {
    "end"
  } else if (benchmark_x <= left + 130) {
    "start"
  } else {
    "middle"
  }
  benchmark_label_x <- benchmark_x + if (benchmark_anchor == "end") {
    -5
  } else if (benchmark_anchor == "start") {
    5
  } else {
    0
  }
  benchmark_line <- paste0(
    '<line class="distribution-target" x1="',
    sprintf("%.1f", benchmark_x), '" x2="',
    sprintf("%.1f", benchmark_x), '" y1="', top,
    '" y2="', bottom, '"></line><text class="distribution-label" x="',
    sprintf("%.1f", benchmark_label_x), '" y="18" text-anchor="',
    benchmark_anchor, '">constitutional standard ',
    outlook_format_percent(benchmark, 0L), '</text>'
  )
  ticks <- seq(x_min, x_max, length.out = 4L)
  tick_html <- paste(vapply(seq_along(ticks), function(i) {
    value <- ticks[i]
    anchor <- if (i == 1L) "start" else if (i == length(ticks)) "end" else "middle"
    paste0(
      '<line class="distribution-tick" x1="',
      sprintf("%.1f", x_position(value)), '" x2="',
      sprintf("%.1f", x_position(value)), '" y1="', bottom,
      '" y2="', bottom + 5, '"></line><text class="axis-label" x="',
      sprintf("%.1f", x_position(value)), '" y="145" text-anchor="', anchor, '">',
      outlook_format_percent(value, 0L), '</text>'
    )
  }, character(1)), collapse = "")
  paste0(
    '<svg class="distribution-chart" viewBox="0 0 770 155" role="img" ',
    'aria-label="Distribution of latest provider performance">',
    benchmark_line,
    '<line class="distribution-axis" x1="', left, '" x2="', right,
    '" y1="', bottom, '" y2="', bottom, '"></line>',
    dots, tick_html, '</svg>'
  )
}

outlook_chart_svg <- function(national, forecast_row, months_shown = 13L) {
  target_month <- data.table::as.IDate(forecast_row$forecast_month[1L])
  first_month <- data.table::as.IDate(seq(
    as.Date(target_month), by = "-1 month", length.out = months_shown
  )[months_shown])
  history <- data.table::copy(national[
    calendar_month >= first_month & calendar_month <= target_month
  ])
  data.table::setorder(history, calendar_month)
  actual <- forecast_row$actual_performance[1L]
  point <- forecast_row$predicted_performance[1L]
  lower <- forecast_row$lower_80[1L]
  upper <- forecast_row$upper_80[1L]
  plot_dates <- sort(unique(c(history$calendar_month, target_month)))
  values <- c(history$ae4h_performance, point, lower, upper, actual)
  values <- values[is.finite(values)]
  y_min <- max(0, floor(100 * (min(values) - 0.015)) / 100)
  y_max <- min(1, ceiling(100 * (max(values) + 0.015)) / 100)
  if (y_max - y_min < 0.10) {
    midpoint <- mean(c(y_min, y_max))
    y_min <- max(0, midpoint - 0.05)
    y_max <- min(1, midpoint + 0.05)
  }
  left <- 54
  right <- 728
  top <- 24
  bottom <- 198
  x_position <- function(date) {
    index <- match(as.character(date), as.character(plot_dates))
    if (length(plot_dates) == 1L) return((left + right) / 2)
    left + (index - 1L) * (right - left) / (length(plot_dates) - 1L)
  }
  y_position <- function(value) {
    bottom - (value - y_min) * (bottom - top) / (y_max - y_min)
  }
  history_points <- paste(vapply(seq_len(nrow(history)), function(i) {
    paste0(
      sprintf("%.1f", x_position(history$calendar_month[i])), ",",
      sprintf("%.1f", y_position(history$ae4h_performance[i]))
    )
  }, character(1)), collapse = " ")
  target_x <- x_position(target_month)
  forecast_y <- y_position(point)
  prior_month <- outlook_previous_month(target_month)
  prior <- national[calendar_month == prior_month, ae4h_performance]
  prior_segment <- if (length(prior) == 1L) paste0(
    '<line class="forecast-link" x1="', sprintf("%.1f", x_position(prior_month)),
    '" y1="', sprintf("%.1f", y_position(prior)), '" x2="',
    sprintf("%.1f", target_x), '" y2="', sprintf("%.1f", forecast_y), '"></line>'
  ) else ""
  actual_mark <- if (!is.na(actual)) paste0(
    '<circle class="actual-release-dot" cx="', sprintf("%.1f", target_x),
    '" cy="', sprintf("%.1f", y_position(actual)), '" r="5"></circle>',
    '<text class="chart-label" x="', sprintf("%.1f", target_x - 7),
    '" y="', sprintf("%.1f", y_position(actual) + 20),
    '" text-anchor="end">Actual ', outlook_format_percent(actual), '</text>'
  ) else ""
  ticks <- seq(y_min, y_max, length.out = 3L)
  grid <- paste(vapply(ticks, function(value) paste0(
    '<line class="chart-grid" x1="', left, '" x2="', right,
    '" y1="', sprintf("%.1f", y_position(value)), '" y2="',
    sprintf("%.1f", y_position(value)), '"></line>',
    '<text class="axis-label" x="8" y="', sprintf("%.1f", y_position(value) + 4),
    '">', sprintf("%.0f%%", 100 * value), '</text>'
  ), character(1)), collapse = "")
  tick_indices <- unique(round(seq(1, length(plot_dates), length.out = min(4L, length(plot_dates)))))
  x_ticks <- paste(vapply(seq_along(tick_indices), function(j) {
    i <- tick_indices[j]
    anchor <- if (j == 1L) "start" else if (j == length(tick_indices)) {
      "end"
    } else {
      "middle"
    }
    paste0(
      '<text class="axis-label" x="', sprintf("%.1f", x_position(plot_dates[i])),
      '" y="226" text-anchor="', anchor, '">',
      outlook_format_short_month(plot_dates[i]), '</text>'
    )
  }, character(1)), collapse = "")
  paste0(
    '<svg class="outlook-chart" viewBox="0 0 760 238" role="img" ',
    'aria-label="Recent national all-types four-hour performance and monthly forecast">',
    grid,
    '<polyline class="actual-line" points="', history_points, '"></polyline>',
    prior_segment,
    '<line class="forecast-range" x1="', sprintf("%.1f", target_x),
    '" x2="', sprintf("%.1f", target_x), '" y1="',
    sprintf("%.1f", y_position(upper)), '" y2="', sprintf("%.1f", y_position(lower)),
    '"></line>',
    '<circle class="forecast-dot" cx="', sprintf("%.1f", target_x), '" cy="',
    sprintf("%.1f", forecast_y), '" r="6"></circle>',
    '<text class="chart-label" x="', sprintf("%.1f", target_x - 7), '" y="',
    sprintf("%.1f", forecast_y - 12), '" text-anchor="end">Forecast ',
    outlook_format_percent(point), '</text>',
    actual_mark,
    x_ticks,
    '</svg>'
  )
}

outlook_medium_term_svg <- function(
    national, medium_forecast, prior_target_month, prior_target_performance,
    intermediate_target_month, intermediate_target_performance,
    final_target_start_month, final_target_performance) {
  forecast <- data.table::copy(medium_forecast)
  data.table::setorder(forecast, forecast_month)
  latest_month <- max(national$calendar_month)
  recent_start <- data.table::as.IDate(seq(
    as.Date(latest_month), by = "-1 month", length.out = 49L
  )[49L])
  first_month <- min(recent_start, prior_target_month)
  final_month <- max(forecast$forecast_month)
  history <- data.table::copy(national[
    calendar_month >= first_month & calendar_month <= latest_month
  ])
  left <- 54
  right <- 728
  top <- 22
  bottom <- 206
  first_id <- month_id(first_month)
  final_id <- month_id(final_month)
  x_position <- function(date) {
    left + (month_id(date) - first_id) * (right - left) / (final_id - first_id)
  }
  values <- c(
    history$ae4h_performance, forecast$predicted_performance,
    forecast$lower_80, forecast$upper_80, prior_target_performance,
    intermediate_target_performance, final_target_performance
  )
  values <- values[is.finite(values)]
  y_min <- max(0, floor(20 * (min(values) - 0.02)) / 20)
  y_max <- min(1, ceiling(20 * (max(values) + 0.02)) / 20)
  y_position <- function(value) {
    bottom - (value - y_min) * (bottom - top) / (y_max - y_min)
  }
  points <- function(dates, values) paste(vapply(seq_along(dates), function(i) {
    paste0(
      sprintf("%.1f", x_position(dates[i])), ",",
      sprintf("%.1f", y_position(values[i]))
    )
  }, character(1)), collapse = " ")
  actual_points <- points(history$calendar_month, history$ae4h_performance)
  forecast_dates <- data.table::as.IDate(c(
    as.Date(latest_month), as.Date(forecast$forecast_month)
  ))
  forecast_values <- c(
    national[calendar_month == latest_month, ae4h_performance],
    forecast$predicted_performance
  )
  forecast_points <- points(forecast_dates, forecast_values)
  ribbon <- ""
  if (all(c("lower_80", "upper_80") %in% names(forecast))) {
    ribbon_points <- paste(
      points(forecast$forecast_month, forecast$upper_80),
      points(rev(forecast$forecast_month), rev(forecast$lower_80))
    )
    ribbon <- paste0(
      '<polygon class="planning-ribbon" points="', ribbon_points, '"></polygon>'
    )
  }
  ticks <- seq(y_min, y_max, length.out = 4L)
  grid <- paste(vapply(ticks, function(value) paste0(
    '<line class="chart-grid" x1="', left, '" x2="', right,
    '" y1="', sprintf("%.1f", y_position(value)), '" y2="',
    sprintf("%.1f", y_position(value)), '"></line>',
    '<text class="axis-label" x="8" y="',
    sprintf("%.1f", y_position(value) + 4), '">',
    sprintf("%.0f%%", 100 * value), '</text>'
  ), character(1)), collapse = "")
  tick_dates <- unique(data.table::as.IDate(c(
    as.Date(first_month), as.Date(latest_month),
    as.Date(intermediate_target_month), as.Date(final_target_start_month),
    as.Date(final_month)
  )))
  tick_dates <- tick_dates[tick_dates >= first_month & tick_dates <= final_month]
  x_ticks <- paste(vapply(seq_along(tick_dates), function(i) {
    anchor <- if (i == 1L) "start" else if (i == length(tick_dates)) {
      "end"
    } else {
      "middle"
    }
    paste0(
      '<text class="axis-label" x="', sprintf("%.1f", x_position(tick_dates[i])),
      '" y="235" text-anchor="', anchor, '">',
      outlook_format_short_month(tick_dates[i]), '</text>'
    )
  }, character(1)), collapse = "")
  prior_mark <- paste0(
    '<circle class="target-dot" cx="', sprintf("%.1f", x_position(prior_target_month)),
    '" cy="', sprintf("%.1f", y_position(prior_target_performance)), '" r="4"></circle>',
    '<text class="target-label" x="', sprintf("%.1f", x_position(prior_target_month) + 6),
    '" y="', sprintf("%.1f", y_position(prior_target_performance) - 7), '">',
    sprintf("%.0f%% Mar 2026", 100 * prior_target_performance), '</text>'
  )
  intermediate_mark <- paste0(
    '<circle class="target-dot" cx="',
    sprintf("%.1f", x_position(intermediate_target_month)), '" cy="',
    sprintf("%.1f", y_position(intermediate_target_performance)), '" r="4"></circle>',
    '<text class="target-label" x="',
    sprintf("%.1f", x_position(intermediate_target_month) + 6), '" y="',
    sprintf("%.1f", y_position(intermediate_target_performance) - 7), '">',
    sprintf("%.0f%% by Mar 2027", 100 * intermediate_target_performance), '</text>'
  )
  final_mark <- paste0(
    '<line class="target-line" x1="',
    sprintf("%.1f", x_position(final_target_start_month)), '" x2="',
    sprintf("%.1f", x_position(final_month)), '" y1="',
    sprintf("%.1f", y_position(final_target_performance)), '" y2="',
    sprintf("%.1f", y_position(final_target_performance)), '"></line>',
    '<text class="target-label" x="',
    sprintf("%.1f", x_position(final_target_start_month) + 6), '" y="',
    sprintf("%.1f", y_position(final_target_performance) - 7), '">',
    sprintf("%.0f%% in 2028/29", 100 * final_target_performance), '</text>'
  )
  paste0(
    '<svg class="outlook-chart" viewBox="0 0 760 246" role="img" ',
    'aria-label="National performance, medium-term forecast and planning milestones">',
    grid, ribbon,
    '<polyline class="actual-line" points="', actual_points, '"></polyline>',
    '<polyline class="planning-forecast" points="', forecast_points, '"></polyline>',
    prior_mark, intermediate_mark, final_mark, x_ticks,
    '</svg>'
  )
}

outlook_provider_rows <- function(signals, direction, n = 3L) {
  signal_value <- paste0("sustained_", direction, "_trajectory")
  z <- data.table::copy(signals[signal == signal_value])
  if (!nrow(z)) {
    return('<div class="empty-signal">No providers currently meet this sustained-deviation rule.</div>')
  }
  if (direction == "above") {
    data.table::setorder(z, -six_month_gap_to_trajectory_pp)
  } else {
    data.table::setorder(z, six_month_gap_to_trajectory_pp)
  }
  z <- utils::head(z, n)
  paste(vapply(seq_len(nrow(z)), function(i) {
    consistency <- round(6 * z$six_month_direction_share[i])
    status <- data.table::fcase(
      z$signal_status[i] == "continuing_signal", "Continuing",
      z$signal_status[i] == "new_signal", "New",
      default = "Active"
    )
    evidence <- data.table::fcase(
      z$signal_evidence[i] == "genuine_release_vintages",
        "6 archived release forecasts",
      z$signal_evidence[i] == "mixed_genuine_and_simulated",
        paste0(z$genuine_release_vintages_6m[i], " archived + historical backtest"),
      default = "historical backtest"
    )
    unusual <- if (isTRUE(z$statistically_unusual_6m[i])) {
      '<span class="unusual">Historically unusual</span>'
    } else {
      ""
    }
    paste0(
      '<div class="provider-row">',
      '<div><div class="provider-name">',
      outlook_html_escape(z$analysis_trust_name[i]),
      '</div><div class="provider-context">Latest ',
      outlook_format_percent(z$latest_actual_performance[i]), ' · ',
      consistency, ' of 6 months in this direction · ', status,
      '</div><div class="provider-evidence">', evidence, '</div>', unusual, '</div>',
      '<div class="provider-gap ', direction, '"><strong>',
      outlook_format_pp(z$six_month_gap_to_trajectory_pp[i], signed = TRUE),
      '</strong><span>six-month gap</span></div>',
      '</div>'
    )
  }, character(1)), collapse = "")
}

outlook_fill_template <- function(template, replacements) {
  out <- template
  for (key in names(replacements)) {
    out <- gsub(
      paste0("{{", key, "}}"), replacements[[key]], out,
      fixed = TRUE
    )
  }
  out
}

build_ae_outlook_page <- function(
    national, medium_forecast, forecast_row, provider_signals,
    template_path, output_path,
    target_performance = 0.85, provider_window_months = 6L,
    provider_materiality_pp = 2, provider_direction_share = 5 / 6,
    prior_target_month = data.table::as.IDate("2026-03-01"),
    prior_target_performance = 0.78,
    intermediate_target_month = data.table::as.IDate("2027-03-01"),
    intermediate_target_performance = 0.82,
    final_target_start_month = data.table::as.IDate("2028-04-01"),
    forecast_method = data.table::data.table(),
    forecast_components = data.table::data.table()) {
  required_forecast <- c(
    "forecast_month", "predicted_performance", "lower_80", "upper_80",
    "actual_performance", "data_through_month", "latest_actual_performance"
  )
  assert_columns(forecast_row, required_forecast)
  required_provider <- c(
    "data_through_month", "analysis_trust_name", "latest_actual_performance",
    "six_month_gap_to_trajectory_pp", "six_month_direction_share",
    "statistically_unusual_6m", "signal", "signal_status", "signal_evidence",
    "genuine_release_vintages_6m"
  )
  assert_columns(provider_signals, required_provider)
  if (nrow(forecast_row) != 1L) stop("The outlook requires exactly one national forecast row.")
  target_month <- data.table::as.IDate(forecast_row$forecast_month[1L])
  actual <- forecast_row$actual_performance[1L]
  state <- if (is.na(actual)) "Forecast issued" else "Actual released"
  state_class <- if (is.na(actual)) "forecast-state" else "released-state"
  provider_assessed <- sum(!is.na(provider_signals$six_month_gap_to_trajectory_pp))
  above_n <- sum(provider_signals$signal == "sustained_above_trajectory", na.rm = TRUE)
  below_n <- sum(provider_signals$signal == "sustained_below_trajectory", na.rm = TRUE)
  within_n <- provider_assessed - above_n - below_n
  forecast_error <- if (is.na(actual)) {
    "The published result will be added here without removing the forecast."
  } else {
    error <- 100 * (actual - forecast_row$predicted_performance[1L])
    paste0(
      outlook_format_pp(abs(error)), " ", if (error >= 0) "above" else "below",
      " forecast"
    )
  }
  comparison_value <- if (is.na(actual)) forecast_row$predicted_performance[1L] else actual
  standard_gap <- 100 * (comparison_value - target_performance)
  standard_text <- paste0(
    outlook_format_pp(abs(standard_gap)), " ",
    if (standard_gap >= 0) "above" else "below", " the ",
    outlook_format_percent(target_performance, digits = 0L),
    " 2028/29 milestone"
  )
  actual_value <- if (is.na(actual)) "—" else outlook_format_percent(actual)
  direction_months_required <- ceiling(
    provider_window_months * provider_direction_share - 1e-8
  )
  issued_at <- if (
    "forecast_created_at_utc" %in% names(forecast_row) &&
      !is.na(forecast_row$forecast_created_at_utc[1L])
  ) {
    substr(forecast_row$forecast_created_at_utc[1L], 1L, 10L)
  } else {
    "Not recorded"
  }
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  method_text <- if (is.na(actual) && nrow(forecast_method) == 1L) {
    paste0(
      "Fixed equal-weight ensemble of same-month persistence, damped annual drift ",
      "and a shared-season current-era trend; all three components are required. ",
      "The final fit uses ", forecast_method$final_fit_consecutive_months[1L],
      " months. Accuracy is tested on ",
      forecast_method$backtest_predictions_scored[1L],
      " rolling one-month-ahead forecasts. The 80% range uses ",
      forecast_method$interval_calibration_n[1L],
      " empirical current-era forecast errors. The pre-release commentary ",
      "compares the seasonal component with the two trend-bearing components."
    )
  } else {
    paste(
      "Fixed equal-weight ensemble of same-month persistence, damped annual drift",
      "and a shared-season current-era trend, with empirical forecast-error ranges. ",
      "The pre-release commentary compares the seasonal component with the two ",
      "trend-bearing components."
    )
  }
  replacements <- list(
    PAGE_TITLE = paste0("A&E four-hour outlook · ", outlook_format_month(target_month)),
    TARGET_MONTH = outlook_format_month(target_month),
    NATIONAL_HEADING = if (is.na(actual)) {
      "What the next release is expected to show"
    } else {
      "What was forecast—and what was published"
    },
    STATE_LABEL = state,
    STATE_CLASS = state_class,
    DATA_THROUGH_MONTH = outlook_format_month(forecast_row$data_through_month[1L]),
    FORECAST_ISSUED_DATE = issued_at,
    NATIONAL_FORECAST = outlook_format_percent(forecast_row$predicted_performance[1L]),
    FORECAST_RANGE = paste0(
      outlook_format_percent(forecast_row$lower_80[1L]), "–",
      outlook_format_percent(forecast_row$upper_80[1L])
    ),
    LATEST_PUBLISHED_MONTH = outlook_format_month(
      forecast_row$data_through_month[1L]
    ),
    LATEST_PUBLISHED = outlook_format_percent(
      forecast_row$latest_actual_performance[1L]
    ),
    ACTUAL_VALUE = actual_value,
    FORECAST_ERROR = forecast_error,
    STANDARD_GAP = standard_text,
    NATIONAL_NARRATIVE = outlook_narrative(
      national, medium_forecast, forecast_row,
      intermediate_target_month, intermediate_target_performance,
      forecast_components
    ),
    NATIONAL_CHART = outlook_chart_svg(national, forecast_row),
    MEDIUM_TERM_CHART = outlook_medium_term_svg(
      national, medium_forecast,
      prior_target_month, prior_target_performance,
      intermediate_target_month, intermediate_target_performance,
      final_target_start_month, target_performance
    ),
    PROVIDER_DATA_MONTH = if (nrow(provider_signals)) {
      outlook_format_month(provider_signals$data_through_month[1L])
    } else {
      "Not available"
    },
    PROVIDERS_ASSESSED = as.character(provider_assessed),
    PROVIDERS_ABOVE = as.character(above_n),
    PROVIDERS_BELOW = as.character(below_n),
    PROVIDERS_WITHIN = as.character(within_n),
    PROVIDER_COMMENTARY = outlook_provider_commentary(
      provider_assessed, above_n, below_n, within_n
    ),
    PROVIDER_DISTRIBUTION = outlook_provider_distribution_svg(
      provider_signals, 0.95
    ),
    SIGNAL_EVIDENCE_NOTE = if (
      provider_assessed > 0L && all(
        provider_signals[!is.na(six_month_gap_to_trajectory_pp), signal_evidence] ==
          "genuine_release_vintages"
      )
    ) {
      "All six-month signals use forecasts archived before each monthly release."
    } else {
      "The archive is still building: each signal states whether it uses archived release forecasts, a historical backtest, or both."
    },
    PROVIDER_WINDOW_MONTHS = as.character(provider_window_months),
    PROVIDER_MATERIALITY_PP = outlook_format_pp(provider_materiality_pp),
    PROVIDER_DIRECTION_MONTHS = as.character(direction_months_required),
    ABOVE_ROWS = outlook_provider_rows(provider_signals, "above"),
    BELOW_ROWS = outlook_provider_rows(provider_signals, "below"),
    METHOD_TEXT = outlook_html_escape(method_text),
    GENERATED_AT = format(Sys.time(), "%d %B %Y, %H:%M %Z")
  )
  output <- outlook_fill_template(template, replacements)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(output, output_path, useBytes = TRUE)
  invisible(output_path)
}
