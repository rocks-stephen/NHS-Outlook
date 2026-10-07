national_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

national_planning_milestones <- function(config) {
  data.table::data.table(
    milestone = c("78% by March 2026", "82% by March 2027", "85% in 2028/29"),
    month = c(
      parse_date_setting(config, "prior_target_month"),
      parse_date_setting(config, "intermediate_target_month"),
      parse_date_setting(config, "final_target_start_month")
    ),
    performance = c(
      parse_numeric_setting(config, "prior_target_performance", 0, 1),
      parse_numeric_setting(config, "intermediate_target_performance", 0, 1),
      parse_numeric_setting(config, "target_performance", 0, 1)
    )
  )
}

plot_national_history_forecast <- function(history, forecast, config) {
  milestones <- national_planning_milestones(config)
  final_end <- parse_date_setting(config, "forecast_end_month")
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = forecast,
      ggplot2::aes(x = forecast_month, ymin = lower_95, ymax = upper_95),
      fill = "#DCE6F1"
    ) +
    ggplot2::geom_ribbon(
      data = forecast,
      ggplot2::aes(x = forecast_month, ymin = lower_80, ymax = upper_80),
      fill = "#AFC6DD"
    ) +
    ggplot2::geom_line(
      data = history,
      ggplot2::aes(x = calendar_month, y = ae4h_performance),
      linewidth = 0.55,
      colour = "#333333"
    ) +
    ggplot2::geom_line(
      data = forecast,
      ggplot2::aes(x = forecast_month, y = predicted_performance),
      linewidth = 0.9,
      colour = "#702082"
    ) +
    ggplot2::geom_point(
      data = milestones[1:2],
      ggplot2::aes(x = month, y = performance),
      shape = 21,
      fill = "white",
      colour = "#C00000",
      stroke = 0.8,
      size = 2.6
    ) +
    ggplot2::geom_segment(
      data = milestones[3],
      ggplot2::aes(x = month, xend = final_end, y = performance, yend = performance),
      linetype = "dashed",
      colour = "#C00000",
      linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = as.Date(c("2015-06-01", "2019-05-01", "2023-06-01")),
      linetype = "dotted",
      colour = "#777777",
      linewidth = 0.35
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(100 * x), "%"),
      limits = c(0.4, 1.0),
      breaks = seq(0.4, 1.0, 0.1)
    ) +
    ggplot2::labs(
      title = "all-types A&E four-hour performance: history and reference forecast",
      subtitle = "Shading shows empirical 80% and 95% rolling-error intervals; red markers show the 78%, 82% and 85% planning milestones",
      x = NULL,
      y = "Attendances within four hours",
      caption = "Reference forecast averages seasonal persistence, damped recent drift and a count-weighted current-era trend."
    ) +
    national_plot_theme()
}

plot_national_model_accuracy <- function(comparison) {
  z <- comparison[evaluation_window == "primary_selection"]
  z[, model := factor(model, levels = model[order(rmse_pp, decreasing = TRUE)])]
  ggplot2::ggplot(z, ggplot2::aes(x = model, y = rmse_pp)) +
    ggplot2::geom_col(fill = "#702082", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Recent rolling forecast accuracy",
      subtitle = "Root mean squared error across 1, 3, 6 and 12-month forecasts; targets from June 2025",
      x = NULL,
      y = "RMSE (percentage points)"
    ) +
    national_plot_theme()
}

plot_national_deviations <- function(deviations, config) {
  threshold <- parse_numeric_setting(config, "materiality_threshold_pp", 0)
  z <- data.table::copy(deviations)
  z[, status := data.table::fcase(
    statistically_unusual_1m %in% TRUE & practically_material_1m, "Statistically unusual and material",
    practically_material_1m, "Material only",
    default = "Within thresholds"
  )]
  ggplot2::ggplot(z, ggplot2::aes(x = target_month, y = residual_pp, fill = status)) +
    ggplot2::geom_col(width = 25) +
    ggplot2::geom_hline(yintercept = c(-threshold, threshold), linetype = "dashed", colour = "#555555") +
    ggplot2::scale_fill_manual(values = c(
      "Statistically unusual and material" = "#C00000",
      "Material only" = "#F2A900",
      "Within thresholds" = "#A7A9AC"
    )) +
    ggplot2::labs(
      title = "National performance relative to one-month-ahead predictions",
      subtitle = "Positive values mean performance was higher than predicted",
      x = NULL,
      y = "Actual minus predicted (percentage points)",
      fill = NULL
    ) +
    national_plot_theme()
}

plot_national_next_release <- function(history, next_release, config, months_shown = 36L) {
  z <- utils::tail(data.table::copy(history), months_shown)
  f <- data.table::copy(next_release)
  f[, forecast_month := data.table::as.IDate(forecast_month)]
  intermediate_month <- parse_date_setting(config, "intermediate_target_month")
  comparison_target <- if (f$forecast_month[1L] <= intermediate_month) {
    parse_numeric_setting(config, "intermediate_target_performance", 0, 1)
  } else {
    parse_numeric_setting(config, "target_performance", 0, 1)
  }
  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = z,
      ggplot2::aes(x = calendar_month, y = ae4h_performance),
      linewidth = 0.75,
      colour = "#333333"
    ) +
    ggplot2::geom_point(
      data = z,
      ggplot2::aes(x = calendar_month, y = ae4h_performance),
      size = 1.25,
      colour = "#333333"
    ) +
    ggplot2::geom_errorbar(
      data = f,
      ggplot2::aes(x = forecast_month, ymin = lower_95, ymax = upper_95),
      width = 18,
      linewidth = 0.8,
      colour = "#7A7A7A"
    ) +
    ggplot2::geom_errorbar(
      data = f,
      ggplot2::aes(x = forecast_month, ymin = lower_80, ymax = upper_80),
      width = 18,
      linewidth = 2.2,
      colour = "#702082"
    ) +
    ggplot2::geom_point(
      data = f,
      ggplot2::aes(x = forecast_month, y = predicted_performance),
      shape = 21,
      size = 3.4,
      stroke = 0.8,
      fill = "white",
      colour = "#702082"
    ) +
    ggplot2::geom_hline(
      yintercept = comparison_target,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "#C00000"
    ) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::coord_cartesian(ylim = c(0.50, 0.90)) +
    ggplot2::labs(
      title = paste0(
        "Next release forecast: ",
        format(as.Date(f$forecast_month[1L]), "%B %Y")
      ),
      subtitle = paste0(
        "Point forecast ", sprintf("%.1f%%", 100 * f$predicted_performance[1L]),
        "; thick and thin bars show empirical 80% and 95% ranges; dashed line is the next planning milestone"
      ),
      x = NULL,
      y = "all-types attendances within four hours"
    ) +
    national_plot_theme()
}

plot_national_one_step_track_record <- function(rolling, config) {
  current_start <- parse_date_setting(config, "current_era_start")
  z <- data.table::copy(rolling[
    model == config$reference_model & horizon_months == 1L &
      target_month >= current_start
  ])
  long <- data.table::melt(
    z,
    id.vars = "target_month",
    measure.vars = c("actual_performance", "predicted_performance"),
    variable.name = "series",
    value.name = "performance"
  )
  long[, series := data.table::fcase(
    series == "actual_performance", "Actual",
    default = "One-month-ahead prediction"
  )]
  ggplot2::ggplot(
    long,
    ggplot2::aes(x = target_month, y = performance, colour = series)
  ) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_colour_manual(values = c(
      "Actual" = "#333333",
      "One-month-ahead prediction" = "#702082"
    )) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(
      title = "How the one-month-ahead model has tracked the releases",
      subtitle = "Rolling pseudo-real-time predictions using the latest revised historical series",
      x = NULL,
      y = "all-types attendances within four hours",
      colour = NULL
    ) +
    national_plot_theme()
}

plot_national_reference_components <- function(history, all_forecasts, config) {
  keep_models <- c(
    "seasonal_naive", "seasonal_drift_damped",
    "shared_season_current_trend", "reference_ensemble"
  )
  labels <- c(
    seasonal_naive = "Seasonal persistence",
    seasonal_drift_damped = "Damped recent drift",
    shared_season_current_trend = "Shared season/current trend",
    reference_ensemble = "Reference ensemble"
  )
  z <- data.table::copy(all_forecasts[model %in% keep_models])
  z[, model_label := factor(
    labels[model],
    levels = unname(labels[keep_models])
  )]
  recent <- history[calendar_month >= max(history$calendar_month) - 365 * 2]
  target <- parse_numeric_setting(config, "target_performance", 0, 1)
  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = recent,
      ggplot2::aes(x = calendar_month, y = ae4h_performance),
      colour = "#333333",
      linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = z,
      ggplot2::aes(
        x = forecast_month, y = predicted_performance,
        colour = model_label, linewidth = model_label
      )
    ) +
    ggplot2::geom_hline(
      yintercept = target,
      linetype = "dashed",
      colour = "#C00000",
      linewidth = 0.45
    ) +
    ggplot2::scale_linewidth_manual(values = c(0.65, 0.65, 0.65, 1.1), guide = "none") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::coord_cartesian(ylim = c(0.50, 0.90)) +
    ggplot2::labs(
      title = "The reference forecast spans different views of the trajectory",
      subtitle = "Component spread shows model uncertainty that is separate from the empirical forecast intervals",
      x = NULL,
      y = "all-types attendances within four hours",
      colour = NULL
    ) +
    national_plot_theme()
}
