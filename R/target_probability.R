# Indicative likelihood of meeting the first national planning milestone.
# This is NOT a validated success probability: long-horizon predictive bands
# are converted to approximate tail probabilities by assuming a Normal shape.
# It must not be used to revise published or locked analytical forecasts.

nhs_target_probability <- function(point, lower_80, upper_80, target,
                                   higher_is_better = TRUE) {
  z <- as.numeric(c(point, lower_80, upper_80, target))
  if (length(z) != 4L || any(!is.finite(z)) ||
      lower_80 > point || upper_80 < point ||
      !is.logical(higher_is_better) || length(higher_is_better) != 1L ||
      is.na(higher_is_better)) return(NA_real_)
  # Conservative: use the wider of the two half-intervals to avoid making
  # asymmetric or boundary-clipped intervals artificially too certain.
  width <- max(point - lower_80, upper_80 - point)
  if (!is.finite(width) || width <= 0) return(NA_real_)
  sigma <- width / stats::qnorm(0.90)  # central 80% has +/- 1.28155 sigma
  p <- if (higher_is_better) {
    stats::pnorm(target, mean = point, sd = sigma, lower.tail = FALSE)
  } else {
    stats::pnorm(target, mean = point, sd = sigma, lower.tail = TRUE)
  }
  min(1, max(0, p))
}

nhs_format_target_probability <- function(probability) {
  p <- suppressWarnings(as.numeric(probability))
  if (length(p) != 1L || !is.finite(p) || p < 0 || p > 1) return("Not estimated")
  if (p < 0.01) return("<1%")
  if (p > 0.99) return(">99%")
  paste0(round(100 * p), "%")
}

nhs_augment_target_probabilities <- function(rows, root = ".") {
  out <- data.table::copy(rows)
  out[, `:=`(
    target_probability = NA_real_,
    target_probability_method = "not_estimated"
  )]
  for (i in seq_len(nrow(out))) {
    metric <- as.character(out$metric_id[i])
    path <- if (identical(metric, "ae4h_all")) {
      file.path(root, "output/national/reference_forecast.csv")
    } else {
      file.path(root, "output/core", metric, "national_reference_projection.csv")
    }
    if (!file.exists(path)) next
    projection <- tryCatch(data.table::fread(path), error = function(e) NULL)
    if (is.null(projection) || !all(c(
      "forecast_month", "lower_80", "upper_80"
    ) %in% names(projection))) next
    prediction_column <- if (identical(metric, "ae4h_all")) {
      "predicted_performance"
    } else {
      "predicted_value"
    }
    if (!prediction_column %in% names(projection)) next
    target_month <- as.Date(out$target_month[i])
    matched <- which(as.Date(projection$forecast_month) == target_month)
    if (length(matched) != 1L) next
    j <- matched[1L]
    point <- projection[[prediction_column]][j]
    # Do not silently use a different projection vintage than the overview.
    if (!is.finite(point) || !is.finite(out$projected_target_value[i]) ||
        abs(point - out$projected_target_value[i]) > 1e-7) next
    probability <- nhs_target_probability(
      point = point,
      lower_80 = projection$lower_80[j],
      upper_80 = projection$upper_80[j],
      target = out$target_value[i],
      higher_is_better = isTRUE(out$higher_is_better[i])
    )
    if (is.finite(probability)) {
      out$target_probability[i] <- probability
      out$target_probability_method[i] <- "normal_approx_from_80pct_interval_wider_side"
    }
  }
  out[]
}

# Quiet editorial emphasis: colour is supplementary to the explicit number.
# Middle-range probabilities are deliberately neutral, avoiding traffic lights.
nhs_target_chance_class <- function(probability) {
  p <- suppressWarnings(as.numeric(probability))
  if (length(p) != 1L || !is.finite(p) || p < 0 || p > 1) return("chance-unknown")
  if (p < 0.20) return("chance-low")
  if (p >= 0.80) return("chance-high")
  "chance-neutral"
}

nhs_target_display_value <- function(value, unit, digits = 1L,
                                     trim_whole = FALSE) {
  x <- suppressWarnings(as.numeric(value))
  if (length(x) != 1L || !is.finite(x)) return("Not estimated")
  digits <- as.integer(digits[1L])
  if (is.na(digits) || digits < 0L) digits <- 1L
  if (unit == "proportion") {
    x <- x * 100
    if (trim_whole && abs(x - round(x)) < 1e-8) digits <- 0L
    return(sprintf(paste0("%.", digits, "f%%"), x))
  }
  if (unit == "minutes") {
    if (trim_whole && abs(x - round(x)) < 1e-8) digits <- 0L
    return(sprintf(paste0("%.", digits, "f min"), x))
  }
  if (unit == "count") return(format(round(x), big.mark = ",", scientific = FALSE))
  sprintf(paste0("%.", digits, "f"), x)
}

# Used on all eight pre-release deep-dive pages; the actual-result card
# remains on post-release editions. No model fitting or archive writes here.
nhs_target_card_html <- function(projected, target, target_month,
                                 unit, digits, higher_is_better, probability) {
  month <- if (length(target_month) && !is.na(target_month[1L])) {
    format(as.Date(target_month[1L]), "%b %Y")
  } else {
    "Target date unavailable"
  }
  threshold <- if (isTRUE(higher_is_better)) "&ge;" else "&le;"
  chance <- nhs_format_target_probability(probability)
  chance_class <- nhs_target_chance_class(probability)
  paste0(
    '<div class="card target-date-card" aria-label="', month,
    ' target outlook">',
    '<div class="label">', month, ' target outlook</div>',
    '<div class="target-card-stat"><span>Projected</span><strong>',
    nhs_target_display_value(projected, unit, digits), '</strong></div>',
    '<div class="target-card-stat"><span>Target</span><strong>', threshold,
    nhs_target_display_value(target, unit, digits, trim_whole = TRUE),
    '</strong></div>',
    '<div class="target-card-chance ', chance_class, '"><strong>', chance,
    '</strong><span>Model-implied chance</span></div>',
    '</div>'
  )
}


# Outturn editions must retain the target-date outlook recorded at issue.
# The original next-release forecast/actual comparison stays in the three cards;
# this compact strip is presentation-only and uses the issued snapshot values.
nhs_target_strip_html <- function(projected, target, target_month,
                                  unit, digits, higher_is_better, probability) {
  month <- if (length(target_month) && !is.na(target_month[1L])) {
    format(as.Date(target_month[1L]), "%b %Y")
  } else {
    "Target date unavailable"
  }
  threshold <- if (isTRUE(higher_is_better)) "&ge;" else "&le;"
  paste0(
    '<div class="target-outturn-strip" aria-label="', month,
    ' target-date outlook at issue">',
    '<span class="target-outturn-label">', month, ' outlook at issue</span>',
    '<span>Projected <strong>',
    nhs_target_display_value(projected, unit, digits), '</strong></span>',
    '<span>Target <strong>', threshold,
    nhs_target_display_value(target, unit, digits, trim_whole = TRUE),
    '</strong></span>',
    '<span class="', nhs_target_chance_class(probability), '">',
    'Model-implied chance <strong>',
    nhs_format_target_probability(probability), '</strong></span>',
    '</div>'
  )
}
