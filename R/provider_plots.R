provider_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 10.5) +
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    )
}

empty_provider_plot <- function(title, message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 4) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title.position = "plot")
}

plot_provider_next_release_extremes <- function(next_forecast, config) {
  top_n <- parse_integer_setting(config, "chart_top_n", 1L)
  z <- data.table::copy(next_forecast[!is.na(predicted_performance)])
  if (!nrow(z)) {
    return(empty_provider_plot(
      "Provider next-release forecasts",
      "No eligible provider forecasts were produced."
    ))
  }
  low <- utils::head(z[order(predicted_performance)], top_n)
  high <- utils::head(z[order(-predicted_performance)], top_n)
  z <- unique(data.table::rbindlist(list(low, high)), by = "analysis_trust_id")
  z[, provider_label := paste0(analysis_trust_name, " (", analysis_trust_id, ")")]
  z[, provider_label := factor(
    provider_label,
    levels = provider_label[order(predicted_performance)]
  )]
  ggplot2::ggplot(z, ggplot2::aes(y = provider_label, x = predicted_performance)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lower_95, xend = upper_95, yend = provider_label),
      linewidth = 0.6,
      colour = "#8A8A8A"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lower_80, xend = upper_80, yend = provider_label),
      linewidth = 2.0,
      colour = "#702082"
    ) +
    ggplot2::geom_point(
      shape = 21, size = 2.8, stroke = 0.7, fill = "white", colour = "#702082"
    ) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(
      title = "Highest and lowest provider forecasts for the next release",
      subtitle = "Point forecasts with empirical 80% and 95% ranges; providers are compared with their own history in the watchlist",
      x = "Forecast all-types four-hour performance",
      y = NULL
    ) +
    provider_plot_theme()
}

plot_provider_forecast_distribution <- function(next_forecast, config) {
  z <- next_forecast[!is.na(predicted_performance)]
  if (!nrow(z)) {
    return(empty_provider_plot(
      "Distribution of provider next-release forecasts",
      "No eligible provider forecasts were produced."
    ))
  }
  ggplot2::ggplot(z, ggplot2::aes(x = predicted_performance)) +
    ggplot2::geom_histogram(
      binwidth = 0.025, boundary = 0, fill = "#702082", colour = "white"
    ) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(
      title = "Distribution of provider forecasts for the next release",
      subtitle = "Provider forecasts are shown descriptively; the national milestone is not treated as an equal provider threshold",
      x = "Forecast all-types four-hour performance",
      y = "Providers"
    ) +
    provider_plot_theme()
}

plot_provider_sustained_deviations <- function(watchlist, config) {
  top_n <- parse_integer_setting(config, "chart_top_n", 1L)
  z <- data.table::copy(watchlist[signal %in% c(
    "sustained_above_trajectory", "sustained_below_trajectory"
  )])
  if (!nrow(z)) {
    return(empty_provider_plot(
      "Providers with a sustained departure from trajectory",
      "No provider currently meets the six-month persistence rule."
    ))
  }
  low <- utils::head(z[order(six_month_gap_to_trajectory_pp)], top_n)
  high <- utils::head(z[order(-six_month_gap_to_trajectory_pp)], top_n)
  z <- unique(data.table::rbindlist(list(low, high)), by = "analysis_trust_id")
  z[, provider_label := paste0(analysis_trust_name, " (", analysis_trust_id, ")")]
  z[, provider_label := factor(
    provider_label,
    levels = provider_label[order(six_month_gap_to_trajectory_pp)]
  )]
  z[, signal_label := data.table::fcase(
    signal == "sustained_above_trajectory", "Above own trajectory",
    default = "Below own trajectory"
  )]
  ggplot2::ggplot(
    z,
    ggplot2::aes(
      x = six_month_gap_to_trajectory_pp,
      y = provider_label,
      fill = signal_label
    )
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_vline(xintercept = 0, colour = "#555555", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c(
      "Above own trajectory" = "#2E7D32",
      "Below own trajectory" = "#C00000"
    )) +
    ggplot2::labs(
      title = "Providers with a sustained six-month departure from trajectory",
      subtitle = "Average actual minus one-month-ahead expected performance; at least five of six months must point the same way",
      x = "Six-month mean gap (percentage points)",
      y = NULL,
      fill = NULL
    ) +
    provider_plot_theme()
}

plot_provider_flagged_trajectories <- function(deviations, watchlist, config) {
  max_providers <- parse_integer_setting(config, "chart_top_n", 1L)
  months_shown <- parse_integer_setting(config, "chart_recent_months", 6L)
  selected <- data.table::copy(watchlist[signal %in% c(
    "sustained_above_trajectory", "sustained_below_trajectory"
  )])
  if (!nrow(selected)) {
    return(empty_provider_plot(
      "Actual and expected performance for flagged providers",
      "No provider currently meets the six-month persistence rule."
    ))
  }
  selected[, gap_abs___ := abs(six_month_gap_to_trajectory_pp)]
  data.table::setorder(selected, -gap_abs___)
  selected <- utils::head(selected, max_providers)
  z <- deviations[analysis_trust_id %in% selected$analysis_trust_id]
  latest_month_id <- max(month_id(z$target_month))
  z <- z[month_id(target_month) >= latest_month_id - months_shown + 1L]
  z[, provider_label := paste0(analysis_trust_name, " (", analysis_trust_id, ")")]
  long <- data.table::melt(
    z,
    id.vars = c("analysis_trust_id", "provider_label", "target_month"),
    measure.vars = c("actual_performance", "predicted_performance"),
    variable.name = "series",
    value.name = "performance"
  )
  long[, series := data.table::fcase(
    series == "actual_performance", "Actual",
    default = "Expected from prior month"
  )]
  ggplot2::ggplot(
    long,
    ggplot2::aes(x = target_month, y = performance, colour = series)
  ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::facet_wrap(~ provider_label, ncol = 3) +
    ggplot2::scale_colour_manual(values = c(
      "Actual" = "#333333",
      "Expected from prior month" = "#702082"
    )) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(
      title = "Actual and expected performance for flagged providers",
      subtitle = "A persistent vertical separation is the signal; isolated gaps are not enough",
      x = NULL,
      y = "all-types attendances within four hours",
      colour = NULL
    ) +
    provider_plot_theme()
}

plot_provider_model_accuracy <- function(comparison, config) {
  z <- data.table::copy(comparison[evaluation_window == "current_era"])
  if (!nrow(z)) {
    return(empty_provider_plot(
      "Provider model accuracy",
      "No current-era provider validation results were produced."
    ))
  }
  z[, model := factor(model, levels = model[order(rmse_pp, decreasing = TRUE)])]
  ggplot2::ggplot(z, ggplot2::aes(x = model, y = rmse_pp)) +
    ggplot2::geom_col(fill = "#702082", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Provider one-month-ahead forecast accuracy",
      subtitle = "Current-era rolling errors, with each provider-month weighted equally",
      x = NULL,
      y = "RMSE (percentage points)"
    ) +
    provider_plot_theme()
}
