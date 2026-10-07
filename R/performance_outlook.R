performance_html_escape <- function(x) {
  z <- as.character(x)
  z <- gsub("&", "&amp;", z, fixed = TRUE)
  z <- gsub("<", "&lt;", z, fixed = TRUE)
  z <- gsub(">", "&gt;", z, fixed = TRUE)
  z <- gsub('"', "&quot;", z, fixed = TRUE)
  z
}

performance_format_month <- function(x) {
  if (!length(x) || is.na(x[1L])) return("Not available")
  format(as.Date(x[1L]), "%B %Y")
}

performance_format_short_month <- function(x) {
  if (!length(x) || is.na(x[1L])) return("Not available")
  format(as.Date(x[1L]), "%b %Y")
}

performance_format_value <- function(value, unit, digits = 1L) {
  if (!length(value) || is.na(value[1L])) return("Not available")
  value <- value[1L]
  digits <- as.integer(digits[1L])
  if (unit == "proportion") {
    return(sprintf(paste0("%.", digits, "f%%"), 100 * value))
  }
  if (unit == "minutes") {
    return(sprintf(paste0("%.", digits, "f min"), value))
  }
  if (unit == "count") {
    return(format(round(value), big.mark = ",", scientific = FALSE))
  }
  sprintf(paste0("%.", digits, "f"), value)
}

performance_format_difference <- function(value, unit, digits = 1L, signed = FALSE) {
  if (!length(value) || is.na(value[1L])) return("Not available")
  value <- value[1L]
  digits <- as.integer(digits[1L])
  prefix <- if (signed) "+" else ""
  if (unit == "proportion") {
    format_string <- if (signed) paste0("%+.", digits, "fpp") else paste0("%.", digits, "fpp")
    return(sprintf(format_string, 100 * value))
  }
  suffix <- if (unit == "minutes") " min" else ""
  format_string <- if (signed) paste0("%+.", digits, "f", suffix) else {
    paste0("%.", digits, "f", suffix)
  }
  sprintf(format_string, value)
}

performance_required_row_columns <- function() {
  c(
    "metric_id", "display_order", "display_name", "short_name", "unit", "digits",
    "higher_is_better", "flat_threshold_native", "deep_dive_file",
    "latest_month", "latest_value", "latest_vintage_forecast_value",
    "latest_vintage_lower_80", "latest_vintage_upper_80",
    "latest_vintage_forecast_error", "forecast_month", "forecast_value",
    "lower_80", "upper_80", "forecast_change_native", "expected_direction",
    "target_month", "target_value", "target_label", "projected_target_value",
    "target_gap_native", "trajectory_status", "providers_assessed",
    "providers_favourable", "providers_adverse", "providers_no_signal",
    "forecast_issued_at_utc"
  )
}

performance_latest_scored_forecast <- function(
    scorecard, forecast_version_value, model_value, actual_month_value) {
  if (!nrow(scorecard)) return(data.table::data.table())
  z <- data.table::copy(scorecard[
    forecast_version == forecast_version_value & model == model_value &
      forecast_status == "scored" & forecast_month == actual_month_value &
      !is.na(actual_performance)
  ])
  if (!nrow(z)) return(z)
  if ("forecast_created_at_utc" %in% names(z)) {
    data.table::setorder(z, forecast_created_at_utc)
  }
  z[.N]
}

performance_ae4h_row <- function(
    metric_config, national_config, next_release, reference_forecast,
    release_archive, scorecard, watchlist) {
  if (nrow(metric_config) != 1L) stop("A&E overview adapter requires one metric config row.")
  if (nrow(next_release) != 1L) stop("A&E overview adapter requires one next-release row.")
  required_next <- c(
    "forecast_version", "data_through_month", "forecast_month", "model",
    "predicted_performance", "lower_80", "upper_80", "latest_actual_performance"
  )
  assert_columns(next_release, required_next)
  assert_columns(reference_forecast, c("forecast_month", "predicted_performance"))
  assert_columns(release_archive, c(
    "forecast_version", "data_through_month", "forecast_month", "model"
  ))
  assert_columns(scorecard, c(
    "forecast_version", "forecast_month", "model", "predicted_performance",
    "actual_performance", "forecast_status"
  ))
  assert_columns(watchlist, c(
    "data_through_month", "six_month_gap_to_trajectory_pp", "signal"
  ))

  latest_month <- data.table::as.IDate(next_release$data_through_month[1L])
  forecast_month_value <- data.table::as.IDate(next_release$forecast_month[1L])
  higher_is_better <- isTRUE(metric_config$higher_is_better[1L])
  latest_score <- performance_latest_scored_forecast(
    scorecard,
    next_release$forecast_version[1L],
    next_release$model[1L],
    latest_month
  )
  archive_match <- release_archive[
    forecast_version == next_release$forecast_version[1L] &
      data_through_month == latest_month & forecast_month == forecast_month_value &
      model == next_release$model[1L]
  ]
  if (nrow(archive_match) && "forecast_created_at_utc" %in% names(archive_match)) {
    data.table::setorder(archive_match, forecast_created_at_utc)
  }

  target_month <- parse_date_setting(national_config, "intermediate_target_month")
  target_value <- parse_numeric_setting(
    national_config, "intermediate_target_performance", 0, 1
  )
  target_projection <- reference_forecast[
    forecast_month == target_month, predicted_performance
  ]
  if (length(target_projection) != 1L || !is.finite(target_projection)) {
    stop("A&E overview could not locate one reference projection for the interim target month.")
  }
  target_gap <- target_projection - target_value
  favourable_target_gap <- if (higher_is_better) target_gap else -target_gap

  current_watchlist <- watchlist[data_through_month == latest_month]
  providers_assessed <- sum(!is.na(current_watchlist$six_month_gap_to_trajectory_pp))
  above_n <- sum(
    current_watchlist$signal == "sustained_above_trajectory", na.rm = TRUE
  )
  below_n <- sum(
    current_watchlist$signal == "sustained_below_trajectory", na.rm = TRUE
  )
  favourable_n <- if (higher_is_better) above_n else below_n
  adverse_n <- if (higher_is_better) below_n else above_n
  no_signal_n <- providers_assessed - favourable_n - adverse_n

  latest_value <- next_release$latest_actual_performance[1L]
  forecast_value <- next_release$predicted_performance[1L]
  forecast_change <- forecast_value - latest_value
  favourable_change <- if (higher_is_better) forecast_change else -forecast_change
  threshold <- metric_config$flat_threshold_native[1L]
  expected_direction <- if (abs(favourable_change) < threshold) {
    "broadly_stable"
  } else if (favourable_change > 0) {
    "improving"
  } else {
    "deteriorating"
  }

  data.table::data.table(
    metric_id = metric_config$metric_id[1L],
    display_order = as.integer(metric_config$display_order[1L]),
    display_name = metric_config$display_name[1L],
    short_name = metric_config$short_name[1L],
    unit = metric_config$unit[1L],
    digits = as.integer(metric_config$digits[1L]),
    higher_is_better = higher_is_better,
    flat_threshold_native = threshold,
    deep_dive_file = metric_config$deep_dive_file[1L],
    latest_month = latest_month,
    latest_value = latest_value,
    latest_vintage_forecast_value = if (nrow(latest_score)) {
      latest_score$predicted_performance[1L]
    } else {
      NA_real_
    },
    latest_vintage_forecast_error = if (nrow(latest_score)) {
      latest_value - latest_score$predicted_performance[1L]
    } else {
      NA_real_
    },
    latest_vintage_lower_80 = if (
      nrow(latest_score) && "lower_80" %in% names(latest_score)
    ) latest_score$lower_80[1L] else NA_real_,
    latest_vintage_upper_80 = if (
      nrow(latest_score) && "upper_80" %in% names(latest_score)
    ) latest_score$upper_80[1L] else NA_real_,
    forecast_month = forecast_month_value,
    forecast_value = forecast_value,
    lower_80 = next_release$lower_80[1L],
    upper_80 = next_release$upper_80[1L],
    forecast_change_native = forecast_change,
    expected_direction = expected_direction,
    target_month = target_month,
    target_value = target_value,
    target_label = paste0(
      performance_format_value(target_value, "proportion", 0L), " by ",
      performance_format_month(target_month)
    ),
    projected_target_value = target_projection,
    target_gap_native = target_gap,
    trajectory_status = if (favourable_target_gap >= 0) "on_trajectory" else "off_course",
    providers_assessed = as.integer(providers_assessed),
    providers_favourable = as.integer(favourable_n),
    providers_adverse = as.integer(adverse_n),
    providers_no_signal = as.integer(no_signal_n),
    forecast_issued_at_utc = if (
      nrow(archive_match) &&
        "forecast_created_at_utc" %in% names(archive_match) &&
        !is.na(archive_match$forecast_created_at_utc[nrow(archive_match)])
    ) {
      archive_match$forecast_created_at_utc[nrow(archive_match)]
    } else {
      NA_character_
    }
  )
}

validate_performance_outlook_rows <- function(rows) {
  assert_columns(rows, performance_required_row_columns(), "performance outlook rows")
  problems <- character()
  if (!nrow(rows)) problems <- c(problems, "no active metrics")
  if (anyDuplicated(rows$metric_id)) problems <- c(problems, "duplicate metric IDs")
  if (anyNA(rows[, .(
    metric_id, display_order, display_name, unit, digits, higher_is_better,
    latest_month, latest_value, forecast_month, forecast_value,
    lower_80, upper_80, target_month, target_value, projected_target_value
  )])) problems <- c(problems, "missing required metric values")
  if (nrow(rows) && any(rows$forecast_month <= rows$latest_month)) {
    problems <- c(problems, "forecast months do not follow latest published months")
  }
  if (nrow(rows) && any(
    rows$lower_80 > rows$forecast_value | rows$forecast_value > rows$upper_80
  )) problems <- c(problems, "forecast intervals do not contain point forecasts")
  if (nrow(rows) && any(
    rows$providers_favourable + rows$providers_adverse + rows$providers_no_signal !=
      rows$providers_assessed
  )) problems <- c(problems, "provider signal counts do not reconcile")
  if (length(problems)) {
    stop("Performance outlook validation failed: ", paste(unique(problems), collapse = "; "), ".")
  }
  invisible(TRUE)
}

performance_latest_comparison_text <- function(row) {
  if (is.na(row$latest_vintage_forecast_value)) {
    return("No archived pre-release forecast")
  }
  error <- row$latest_vintage_forecast_error
  paste0(
    performance_format_difference(abs(error), row$unit, row$digits), " ",
    if (error >= 0) "above" else "below", " forecast"
  )
}

performance_target_gap_text <- function(row) {
  gap <- row$target_gap_native
  favourable_gap <- if (isTRUE(row$higher_is_better)) gap else -gap
  direction <- if (favourable_gap >= 0) {
    if (isTRUE(row$higher_is_better)) "above" else "below"
  } else {
    if (isTRUE(row$higher_is_better)) "below" else "above"
  }
  paste0(
    performance_format_difference(abs(gap), row$unit, row$digits), " ",
    direction, " milestone"
  )
}

performance_attach_history <- function(rows, history) {
  required <- c("metric_id", "calendar_month", "value")
  assert_columns(history, required, "performance history")
  out <- data.table::copy(rows)
  out[, `:=`(
    previous_month = data.table::as.IDate(NA_character_),
    previous_value = NA_real_
  )]
  for (i in seq_len(nrow(out))) {
    z <- data.table::copy(history[
      metric_id == out$metric_id[i] & calendar_month < out$latest_month[i] &
        is.finite(value)
    ])
    data.table::setorder(z, calendar_month)
    if (nrow(z)) {
      out$previous_month[i] <- z$calendar_month[nrow(z)]
      out$previous_value[i] <- z$value[nrow(z)]
    }
  }
  out[]
}

performance_prepare_edition <- function(rows, history, edition = "forecast") {
  if (!edition %in% c("forecast", "outturn")) {
    stop("Edition must be 'forecast' or 'outturn'.")
  }
  out <- performance_attach_history(rows, history)
  out[, `:=`(
    edition = edition,
    actual_month = data.table::as.IDate(NA_character_),
    actual_value = NA_real_,
    actual_error_native = NA_real_
  )]
  if (edition == "forecast") return(out[])
  out <- out[
    is.finite(latest_vintage_forecast_value) & is.finite(previous_value) &
      !is.na(latest_vintage_lower_80) & !is.na(latest_vintage_upper_80)
  ]
  if (!nrow(out)) return(out)
  out[, `:=`(
    actual_month = latest_month,
    actual_value = latest_value,
    actual_error_native = latest_value - latest_vintage_forecast_value,
    latest_month = previous_month,
    latest_value = previous_value,
    # RHS expressions in a data.table := call are evaluated before assignment;
    # use the original latest_month directly rather than the pre-existing blank
    # actual_month column.
    forecast_month = latest_month,
    forecast_value = latest_vintage_forecast_value,
    lower_80 = latest_vintage_lower_80,
    upper_80 = latest_vintage_upper_80
  )]
  out[, forecast_change_native := forecast_value - latest_value]
  out[, expected_direction := {
    favourable <- if (higher_is_better) forecast_change_native else -forecast_change_native
    if (abs(favourable) < flat_threshold_native) {
      "broadly_stable"
    } else if (favourable > 0) {
      "improving"
    } else {
      "deteriorating"
    }
  }, by = metric_id]
  out[]
}

performance_prepare_published_outturn <- function(
    published_forecast, history, require_all = TRUE) {
  required <- c(
    performance_required_row_columns(), "publication_issue_date"
  )
  assert_columns(
    published_forecast, required, "published forecast snapshot"
  )
  assert_columns(history, c("metric_id", "calendar_month", "value"), "history")
  out <- data.table::copy(published_forecast)
  out[, forecast_month := data.table::as.IDate(forecast_month)]
  actuals <- data.table::copy(history[is.finite(value), .(
    metric_id,
    forecast_month = data.table::as.IDate(calendar_month),
    published_actual_value = value
  )])
  if (anyDuplicated(actuals[, .(metric_id, forecast_month)])) {
    stop("History contains duplicate metric-month rows while preparing the outturn.")
  }
  drop_columns <- intersect(
    c("edition", "actual_month", "actual_value", "actual_error_native"),
    names(out)
  )
  if (length(drop_columns)) out[, (drop_columns) := NULL]
  out <- merge(
    out, actuals,
    by = c("metric_id", "forecast_month"), all.x = TRUE, sort = FALSE
  )
  missing <- out[!is.finite(published_actual_value), .(
    metric_id, display_name, forecast_month
  )]
  if (nrow(missing) && require_all) {
    detail <- paste0(
      missing$display_name, " (", format(as.Date(missing$forecast_month), "%b %Y"), ")"
    )
    stop(
      "Outturn mode requested, but the newly published observation was not imported for: ",
      paste(detail, collapse = ", "),
      ". Rerun from stage 1 after the official files are available."
    )
  }
  out <- out[is.finite(published_actual_value)]
  out[, `:=`(
    edition = "outturn",
    actual_month = forecast_month,
    actual_value = published_actual_value,
    actual_error_native = published_actual_value - forecast_value
  )]
  out[, published_actual_value := NULL]
  data.table::setorder(out, display_order)
  out[]
}

performance_outturn_note <- function(row) {
  if (is.na(row$actual_value)) return("Not yet released")
  favourable_error <- if (isTRUE(row$higher_is_better)) {
    row$actual_error_native
  } else {
    -row$actual_error_native
  }
  interval <- if (
    row$actual_value >= row$lower_80 && row$actual_value <= row$upper_80
  ) "inside the 80% range" else "outside the 80% range"
  paste0(
    performance_format_difference(
      abs(row$actual_error_native), row$unit, row$digits
    ), " ", if (favourable_error >= 0) "better" else "worse",
    " than outlook · ", interval
  )
}

performance_outlook_table_rows <- function(rows) {
  data.table::setorder(rows, display_order)
  pieces <- character()
  previous_group <- ""
  for (i in seq_len(nrow(rows))) {
    row <- rows[i]
    group <- if ("overview_group" %in% names(row)) row$overview_group[1L] else ""
    if (!is.na(group) && nzchar(group) && group != previous_group) {
      pieces <- c(pieces, paste0(
        '<tr class="group-row"><th colspan="5">',
        performance_html_escape(group), '</th></tr>'
      ))
      previous_group <- group
    }
    movement <- if (row$expected_direction == "improving") {
      "Improvement expected"
    } else if (row$expected_direction == "deteriorating") {
      "Deterioration expected"
    } else {
      "Broadly stable"
    }
    benchmark <- if (
      "benchmark_label" %in% names(row) && !is.na(row$benchmark_label) &&
        nzchar(row$benchmark_label)
    ) row$benchmark_label else row$target_label
    provider_note <- if (row$providers_assessed > 0L) paste0(
      row$providers_favourable, " favourable · ", row$providers_adverse,
      " adverse provider signals"
    ) else "Provider trajectory watch not available"
    metric_label <- if (
      "edition" %in% names(row) && row$edition[1L] == "outturn"
    ) paste0(
      '<span class="metric-name">',
      performance_html_escape(row$display_name), '</span>'
    ) else paste0(
      '<a class="metric-name" href="',
      performance_html_escape(row$deep_dive_file), '">',
      performance_html_escape(row$display_name), '</a>'
    )
    actual_cell <- if (!is.na(row$actual_value)) paste0(
      '<span class="cell-value actual-value">',
      performance_format_value(row$actual_value, row$unit, row$digits),
      '</span><span class="cell-date">',
      performance_format_short_month(row$actual_month),
      '</span><span class="cell-note">',
      performance_html_escape(performance_outturn_note(row)), '</span>'
    ) else paste0(
      '<span class="cell-value pending">—</span>',
      '<span class="cell-date">Not yet released</span>',
      '<span class="cell-note">Added in the outturn edition</span>'
    )
    pieces <- c(pieces, paste0(
      '<tr class="metric-row">',
      '<th scope="row">', metric_label,
      '<span class="metric-benchmark">', performance_html_escape(benchmark),
      '</span><span class="metric-signal">',
      performance_html_escape(provider_note), '</span></th>',
      '<td><span class="cell-value">',
      performance_format_value(row$latest_value, row$unit, row$digits),
      '</span><span class="cell-date">',
      performance_format_short_month(row$latest_month),
      '</span><span class="cell-note">Baseline when outlook was made</span></td>',
      '<td class="forecast-cell"><span class="cell-value">',
      performance_format_value(row$forecast_value, row$unit, row$digits),
      '</span><span class="cell-date">',
      performance_format_short_month(row$forecast_month), ' · ', movement,
      '</span><span class="cell-note">80% range ',
      performance_format_value(row$lower_80, row$unit, row$digits), "–",
      performance_format_value(row$upper_80, row$unit, row$digits),
      '</span></td><td>', actual_cell, '</td>',
      '<td class="detail-cell"><a href="',
      performance_html_escape(row$deep_dive_file), '">View →</a></td></tr>'
    ))
  }
  paste(pieces, collapse = "")
}

performance_outlook_commentary <- function(rows, edition = "forecast") {
  data.table::setorder(rows, display_order)
  n_metrics <- nrow(rows)
  improving <- sum(rows$expected_direction == "improving")
  deteriorating <- sum(rows$expected_direction == "deteriorating")
  stable <- sum(rows$expected_direction == "broadly_stable")
  off_course <- sum(rows$trajectory_status == "off_course")
  if (edition == "outturn") {
    if (!n_metrics) return("No archived forecasts are ready to score.")
    inside <- sum(
      rows$actual_value >= rows$lower_80 & rows$actual_value <= rows$upper_80,
      na.rm = TRUE
    )
    favourable_error <- data.table::fifelse(
      rows$higher_is_better,
      rows$actual_error_native,
      -rows$actual_error_native
    )
    return(paste0(
      n_metrics, " newly published results can be compared with genuine ",
      "pre-release forecasts. ", inside, " fell inside their 80% expected range; ",
      sum(favourable_error > 0, na.rm = TRUE), " were better than forecast and ",
      sum(favourable_error < 0, na.rm = TRUE), " were worse."
    ))
  }
  if (n_metrics == 1L) {
    row <- rows[1L]
    movement <- if (row$expected_direction == "improving") {
      "improve"
    } else if (row$expected_direction == "deteriorating") {
      "deteriorate"
    } else {
      "remain broadly stable"
    }
    target_sentence <- if (row$trajectory_status == "off_course") {
      paste0(
        "The medium-term projection remains off course for the ",
        row$target_label, " milestone."
      )
    } else {
      paste0("The medium-term projection is on trajectory for ", row$target_label, ".")
    }
    provider_sentence <- if (row$providers_adverse > row$providers_favourable) {
      paste0(
        "Sustained adverse provider signals outnumber favourable signals (",
        row$providers_adverse, " versus ", row$providers_favourable, ")."
      )
    } else if (row$providers_favourable > row$providers_adverse) {
      paste0(
        "Sustained favourable provider signals outnumber adverse signals (",
        row$providers_favourable, " versus ", row$providers_adverse, ")."
      )
    } else {
      paste0(
        "Sustained favourable and adverse provider signals are balanced at ",
        row$providers_favourable, " each."
      )
    }
    return(paste0(
      row$display_name, " is expected to ", movement, " in ",
      performance_format_month(row$forecast_month), ", from ",
      performance_format_value(row$latest_value, row$unit, row$digits), " to ",
      performance_format_value(row$forecast_value, row$unit, row$digits), ". ",
      target_sentence, " ", provider_sentence
    ))
  }
  movement_parts <- c(
    if (improving) paste(improving, "improving") else character(),
    if (stable) paste(stable, "broadly stable") else character(),
    if (deteriorating) paste(deteriorating, "deteriorating") else character()
  )
  paste0(
    "Across ", n_metrics, " headline measures, the next-release outlook is ",
    paste(movement_parts, collapse = ", "), ". ", off_course, " of ", n_metrics,
    " measures remain off course against their medium-term milestones."
  )
}

performance_fill_template <- function(template, replacements) {
  out <- template
  for (key in names(replacements)) {
    out <- gsub(paste0("{{", key, "}}"), replacements[[key]], out, fixed = TRUE)
  }
  out
}

build_performance_outlook_page <- function(rows, template_path, output_path,
                                           excluded_metrics = character(),
                                           edition = "forecast",
                                           editorial_commentary = "",
                                           publication_issue_date = Sys.Date(),
                                           publication_status =
                                             nhs_outlook_publication_status()) {
  validate_performance_outlook_rows(rows)
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  issue_date <- format(as.Date(publication_issue_date), "%d %B %Y")
  latest_months <- unique(vapply(
    seq_len(nrow(rows)),
    function(i) performance_format_month(rows$latest_month[i]),
    character(1)
  ))
  coverage_note <- if (length(latest_months) == 1L) {
    paste0("Latest data through ", latest_months, ".")
  } else {
    "Release months differ by measure; each row is dated."
  }
  if (length(excluded_metrics)) {
    coverage_note <- paste0(
      coverage_note, " Not shown for insufficient history: ",
      paste(excluded_metrics, collapse = ", "), "."
    )
  }
  edition_label <- nhs_outlook_edition_label(edition, publication_status)
  summary <- if (!is.na(editorial_commentary) && nzchar(trimws(editorial_commentary))) {
    editorial_commentary
  } else {
    performance_outlook_commentary(rows, edition)
  }
  source_rows <- unique(rows[
    !is.na(data_source_url) & nzchar(data_source_url),
    .(data_source_label, data_source_url)
  ])
  sources_html <- paste(vapply(seq_len(nrow(source_rows)), function(i) paste0(
    '<a href="', performance_html_escape(source_rows$data_source_url[i]), '">',
    performance_html_escape(source_rows$data_source_label[i]), '</a>'
  ), character(1)), collapse = " · ")
  output <- performance_fill_template(template, list(
    PAGE_TITLE = paste0("NHS Outlook · ", edition_label),
    EDITION_LABEL = edition_label,
    PUBLICATION_STATUS_CLASS = if (publication_status == "pilot") {
      " pilot"
    } else {
      ""
    },
    SUBTITLE = if (edition == "forecast") {
      "What the next monthly releases are expected to show"
    } else {
      "What was published, compared with the pre-release outlook"
    },
    ISSUE_DATE = issue_date,
    N_METRICS = as.character(nrow(rows)),
    COVERAGE_NOTE = coverage_note,
    SUMMARY_COMMENTARY = performance_html_escape(summary),
    METRIC_ROWS = performance_outlook_table_rows(data.table::copy(rows)),
    SOURCES = sources_html,
    GENERATED_AT = format(Sys.time(), "%d %B %Y, %H:%M %Z")
  ))
  unresolved <- regmatches(output, gregexpr("\\{\\{[A-Z0-9_]+\\}\\}", output))[[1L]]
  if (length(unresolved) && unresolved[1L] != "") {
    stop("Unresolved performance outlook template fields: ", paste(unique(unresolved), collapse = ", "))
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(output, output_path, useBytes = TRUE)
  invisible(output_path)
}
