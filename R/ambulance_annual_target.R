# NHS Outlook: Category 2 annual-average outlook (2026/27)
# The 25-minute objective applies to the FINANCIAL-YEAR incident-weighted mean.
# All calculations use the precise national monthly values; do not round them
# before combining. Counts from official returns are weights, not equal months.
# Simulated probabilities are conditional on the monthly forecast distribution,
# seasonal counts and assumed serial correlation: provisional, not validated.

nhs_cat2_month_number <- function(x) {
  d <- as.Date(x)
  as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
}

nhs_cat2_load_annual_series <- function(
    panel_path = "data-interim/core/national_panel.csv",
    projection_path = "output/core/ambulance_cat2/national_reference_projection.csv",
    financial_year_end = as.Date("2027-03-01")) {
  if (!file.exists(panel_path) || !file.exists(projection_path)) {
    stop("Missing monthly Cat2 data or model projection; complete stage 15 first.")
  }
  data <- data.table::fread(panel_path, encoding = "UTF-8")
  required <- c("metric_id", "calendar_month", "value", "denominator")
  if (!all(required %in% names(data))) {
    stop("Cannot construct incident-weighted Category 2 average: national panel lacks ",
         paste(setdiff(required, names(data)), collapse = ", "),
         ". Import actual Category 2 incident counts from AmbSYS; do not use ",
         "a simple mean of monthly means.")
  }
  observed <- data[metric_id == "ambulance_cat2"]
  observed[, calendar_month := as.Date(calendar_month)]
  observed <- observed[is.finite(value) & is.finite(denominator) & denominator > 0]
  data.table::setorder(observed, calendar_month)
  if (!nrow(observed) || anyDuplicated(observed$calendar_month)) {
    stop("Category 2 historical months missing or duplicated.")
  }
  # Supplement the series with independently verified official historical A10/A30
  # totals for rolling 12-month averages. Do not rewrite old modelling inputs.
  history_path <- "output/qa/ambulance_cat2_official_history.csv"
  if (file.exists(history_path)) {
    hist <- data.table::fread(history_path)
    hist[, calendar_month := as.Date(calendar_month)]
    hist <- hist[calendar_month < min(observed$calendar_month)]
    if (nrow(hist)) {
      history_rows <- hist[, .(calendar_month,
                               value = precise_minutes,
                               denominator = incident_count,
                               numerator = response_seconds / 60)]
      observed <- data.table::rbindlist(list(history_rows, observed),
                                        fill = TRUE, use.names = TRUE)
      data.table::setorder(observed, calendar_month)
    }
  }
  latest <- max(observed$calendar_month)
  fy_start <- as.Date("2026-04-01")
  financial_year_end <- as.Date(financial_year_end)
  if (latest < fy_start || latest >= financial_year_end) {
    stop("Expected partially observed 2026/27 (April to October 2026). Latest is ", latest)
  }
  # Only the *financial-year to date* must be complete for the FY target.
  # Older historical counts may be missing; do not mistake that for a missing FY month.
  fy_observed_months <- seq.Date(fy_start, latest, by = "month")
  missing_fy_counts <- setdiff(as.character(fy_observed_months),
                               as.character(observed$calendar_month))
  if (length(missing_fy_counts)) {
    stop("Missing observed England Category 2 incident counts for financial-year " ,
         "months: ", paste(missing_fy_counts, collapse = ", "),
         ". These months need their OFFICIAL AmbSYS incident counts before an " ,
         "incident-weighted annual average can be published. " ,
         "Do not replace observed weights with projected counts.")
  }
  # Trust the exact numerator/denominator when already reconciled. The value
  # column can otherwise contain rounded display values in historical imports.
  observed[, monthly_mean_minutes := as.numeric(value)]
  if ("numerator" %in% names(observed)) {
    usable <- is.finite(observed$numerator) & is.finite(observed$denominator) &
      observed$denominator > 0 &
      abs(observed$numerator / observed$denominator - observed$value) <= .06
    observed$monthly_mean_minutes[usable] <-
      observed$numerator[usable] / observed$denominator[usable]
  }
  observed <- observed[, .(calendar_month,
                            incident_count = as.numeric(denominator),
                            monthly_mean_minutes,
                            kind = "actual")]

  proj <- data.table::fread(projection_path, encoding = "UTF-8")
  required_proj <- c("forecast_month", "predicted_value", "lower_80", "upper_80")
  if (!all(required_proj %in% names(proj))) {
    stop("Cat2 reference projection lacks: ",
         paste(setdiff(required_proj, names(proj)), collapse = ", "),
         ". Annual-average probability cannot be calculated from a point forecast alone.")
  }
  proj[, forecast_month := as.Date(forecast_month)]
  if ("model" %in% names(proj) && anyDuplicated(proj$forecast_month)) {
    proj <- proj[model == "reference_ensemble"]
  }
  proj <- proj[forecast_month > latest]
  data.table::setorder(proj, forecast_month)
  if (!nrow(proj) || anyDuplicated(proj$forecast_month) ||
      any(diff(nhs_cat2_month_number(proj$forecast_month)) != 1L)) {
    stop("Category 2 projection must have one consecutive row per forecast month.")
  }
  remaining_months <- seq.Date(
    as.Date(format(latest + 35, "%Y-%m-01")), financial_year_end, by = "month")
  if (!all(remaining_months %in% proj$forecast_month)) {
    stop("Category 2 projection is missing at least one Nov 2026–Mar 2027 month.")
  }
  if (any(!is.finite(proj$predicted_value) |
          !is.finite(proj$lower_80) | !is.finite(proj$upper_80) |
          proj$upper_80 <= proj$lower_80)) {
    stop("Future monthly forecasts or 80% intervals are missing/invalid.")
  }

  # Forecast weights are a nuisance projection, not observed counts.
  # Prefer same-calendar-month counts from recent prior years. For a month
  # without such a comparator (e.g. Nov 2026 when only Apr-Oct counts exist),
  # fall back transparently to the recent six OBSERVED months' mean.
  # Do not use a future forecast month's count as if it were observed.
  future <- proj[, .(calendar_month = forecast_month,
                     monthly_mean_minutes = as.numeric(predicted_value),
                     lower_80 = as.numeric(lower_80),
                     upper_80 = as.numeric(upper_80))]
  count_estimates <- lapply(as.Date(future$calendar_month), function(d) {
    history <- observed[calendar_month < d]
    same_month <- history[
      as.integer(format(calendar_month, "%m")) ==
        as.integer(format(d, "%m")) &
        calendar_month >= (d - 1120), incident_count]
    if (length(same_month)) {
      return(list(count = mean(utils::tail(same_month, 3L)),
                  method = "historical_same_calendar_month",
                  n_observed_counts = length(same_month)))
    }
    recent <- utils::tail(history$incident_count, 6L)
    if (length(recent) < 3L || any(!is.finite(recent)) || any(recent <= 0)) {
      stop("No comparable same-month count and insufficient recent observed ",
           "Category 2 incident counts for projected month ", d,
           ". Need at least three valid observed monthly counts; " ,
           "inspect data-interim/core/national_panel.csv.")
    }
    list(count = mean(recent),
         method = "recent_six_observed_months_mean_no_seasonal_match",
         n_observed_counts = length(recent))
  })
  future[, `:=`(
    incident_count = vapply(count_estimates, `[[`, numeric(1), "count"),
    incident_count_method = vapply(count_estimates, `[[`, character(1), "method"),
    incident_count_history_n = vapply(count_estimates, `[[`, integer(1), "n_observed_counts")
  )]
  future[, kind := "forecast"]
  full <- data.table::rbindlist(list(
    observed[, .(calendar_month, incident_count, monthly_mean_minutes, kind,
                 incident_count_method = "observed_official",
                 incident_count_history_n = NA_integer_)],
    future[, .(calendar_month, incident_count, monthly_mean_minutes, kind,
               incident_count_method, incident_count_history_n)]
  ), use.names = TRUE)
  data.table::setorder(full, calendar_month)
  full[, rolling_12m_minutes := NA_real_]
  full[, month_index := nhs_cat2_month_number(calendar_month)]
  for (i in seq_len(nrow(full))) {
    window <- full[month_index >= full$month_index[i] - 11L &
                   month_index <= full$month_index[i]]
    if (nrow(window) == 12L && all(diff(window$month_index) == 1L)) {
      full$rolling_12m_minutes[i] <-
        sum(window$monthly_mean_minutes * window$incident_count) /
        sum(window$incident_count)
    }
  }
  year <- full[calendar_month >= fy_start &
               calendar_month <= financial_year_end]
  if (nrow(year) != 12L || any(diff(year$month_index) != 1L)) {
    stop("Incomplete FY 2026/27 Category 2 series after combining actuals/forecasts.")
  }
  completed <- year[kind == "actual"]
  uncompleted <- year[kind == "forecast"]
  # Numerator here is total minutes of response, not mean minutes.
  used_minutes <- sum(completed$monthly_mean_minutes * completed$incident_count)
  projected_minutes <- sum(uncompleted$monthly_mean_minutes * uncompleted$incident_count)
  year_incidents <- sum(year$incident_count)
  fy_mean <- (used_minutes + projected_minutes) / year_incidents
  # Required weighted monthly mean for remaining months (if all else fixed).
  remaining_mean_needed <- (25 * year_incidents - used_minutes) /
                           sum(uncompleted$incident_count)

  # Sensitivity to serial dependence matters for multi-month target events.
  # No independence assumption: baseline AR(1) rho=.65, low/high .0/.90.
  horizon <- uncompleted$calendar_month
  uncertainty <- merge(uncompleted[, .(calendar_month, incident_count,
                                       monthly_mean_minutes)],
                       future[, .(calendar_month, lower_80, upper_80)],
                       by = "calendar_month", sort = TRUE)
  sigma <- (uncertainty$upper_80 - uncertainty$lower_80) /
    (2 * stats::qnorm(.9))
  if (any(!is.finite(sigma) | sigma <= 0)) {
    stop("Invalid Cat2 forecast interval width for remaining FY months.")
  }
  simulate <- function(rho, n = 50000L) {
    set.seed(20261112L)
    k <- nrow(uncertainty)
    corr <- outer(seq_len(k), seq_len(k), function(i, j) rho ^ abs(i - j))
    z <- matrix(stats::rnorm(n * k), nrow = n, ncol = k) %*% chol(corr)
    # Use unbounded normal forecast uncertainty; values near 0 not plausible at
    # the observed level. This is a provisional approximation only.
    draw <- sweep(z, 2L, sigma, `*`)
    draw <- sweep(draw, 2L, uncertainty$monthly_mean_minutes, `+`)
    forecast_total <- as.vector(draw %*% uncertainty$incident_count)
    mean((used_minutes + forecast_total) / year_incidents <= 25)
  }
  p_lo <- simulate(0)
  p_ref <- simulate(.65)
  p_hi <- simulate(.90)
  result <- data.table::data.table(
    metric_id = "ambulance_cat2", target_month = financial_year_end,
    target_year = "2026/27", target_value = 25,
    financial_year_mean_forecast = fy_mean,
    annual_probability = p_ref,
    probability_rho_zero = p_lo,
    probability_rho_090 = p_hi,
    observed_year_to_date_minutes = used_minutes / sum(completed$incident_count),
    required_mean_remaining_months = remaining_mean_needed,
    observed_months = nrow(completed), future_months = nrow(uncompleted),
    count_forecast_method = if (any(uncompleted$incident_count_method ==
                "recent_six_observed_months_mean_no_seasonal_match")) {
      "Same-month historical counts where available; otherwise mean of latest six observed monthly counts (nonseasonal fallback)"
    } else "Same-month average of up to 3 historical years",
    n_fy_months_with_count_fallback = sum(uncompleted$incident_count_method ==
                "recent_six_observed_months_mean_no_seasonal_match"),
    probability_method = "Normal monthly 80% ranges; AR(1) residual rho=0.65; projected incident weights (with explicit fallback); 50000 simulations; unvalidated"
  )
  list(series = full, summary = result, projected_months = future)
}

nhs_cat2_save_annual <- function(x, out_dir = "output/core/ambulance_cat2") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(x$series, file.path(out_dir, "annual_rolling_12m_projection.csv"))
  data.table::fwrite(x$summary, file.path(out_dir, "annual_target_outlook.csv"))
  invisible(x)
}

nhs_cat2_apply_annual_to_rows <- function(rows, annual) {
  i <- which(rows$metric_id == "ambulance_cat2")
  if (length(i) != 1L) stop("Expected one ambulance Cat2 row.")
  s <- annual$summary[1L]
  data.table::set(rows, i = i, j = "target_month", value = s$target_month)
  data.table::set(rows, i = i, j = "target_value", value = s$target_value)
  data.table::set(rows, i = i, j = "projected_target_value",
                  value = s$financial_year_mean_forecast)
  data.table::set(rows, i = i, j = "target_probability",
                  value = s$annual_probability)
  if ("target_label" %in% names(rows)) {
    data.table::set(rows, i = i, j = "target_label",
                    value = "2026/27 financial-year average: 25 minutes")
  }
  if ("target_probability_method" %in% names(rows)) {
    data.table::set(rows, i = i, j = "target_probability_method",
                    value = s$probability_method)
  }
  rows[]
}

# Publication chart replaces the busy monthly+annual overlay with a dedicated
# incident-weighted annual-average panel. The short-term monthly chart remains.
nhs_cat2_overlay_annual_chart <- function(html_path, annual) {
  source("R/ambulance_cat2_history_chart.R", local = TRUE)
  nhs_cat2_draw_annual_chart(html_path, annual)
}
