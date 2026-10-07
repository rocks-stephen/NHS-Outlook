release_qa_check <- function(section, metric_id, check, severity, passed,
                             detail, evidence_file = NA_character_) {
  passed_value <- if (length(passed) && !is.na(passed[1L])) {
    isTRUE(passed[1L])
  } else {
    NA
  }
  if (identical(as.character(severity), "fatal") && is.na(passed_value)) {
    passed_value <- FALSE
  }
  data.table::data.table(
    section = as.character(section),
    metric_id = as.character(metric_id),
    check = as.character(check),
    severity = as.character(severity),
    passed = as.logical(passed_value),
    status = if (is.na(passed_value)) {
      "review"
    } else if (passed_value) {
      "passed"
    } else {
      "failed"
    },
    detail = as.character(detail),
    evidence_file = as.character(evidence_file),
    reviewer = NA_character_,
    reviewed_at_utc = NA_character_,
    review_notes = NA_character_
  )
}

release_qa_count_pattern <- function(text, pattern) {
  hits <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (length(hits) == 1L && hits[1L] == -1L) 0L else length(hits)
}

release_qa_read_binary <- function(path) {
  size <- file.info(path)$size
  if (!is.finite(size) || size <= 0) return(raw())
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = as.integer(size))
}

release_qa_raw_positions <- function(bytes, pattern) {
  needle <- charToRaw(pattern)
  if (!length(bytes) || length(bytes) < length(needle)) return(integer())
  candidates <- which(bytes == needle[1L])
  candidates[vapply(candidates, function(position) {
    end <- position + length(needle) - 1L
    end <= length(bytes) && identical(bytes[position:end], needle)
  }, logical(1))]
}

release_qa_pdf_metadata <- function(path) {
  if (!file.exists(path) || file.info(path)$size <= 0) {
    return(list(pages = 0L, width_points = NA_real_, height_points = NA_real_))
  }
  bytes <- release_qa_read_binary(path)
  page_positions <- unique(c(
    release_qa_raw_positions(bytes, "/Type /Page"),
    release_qa_raw_positions(bytes, "/Type/Page")
  ))
  page_positions <- page_positions[vapply(page_positions, function(position) {
    # Exclude the PDF page-tree object '/Pages'.
    suffix_position <- position + if (identical(
      bytes[position:(position + 5L)], charToRaw("/Type/"))) 10L else 11L
    suffix_position > length(bytes) || bytes[suffix_position] != charToRaw("s")
  }, logical(1))]
  pages <- length(page_positions)
  media_pattern <- paste0(
    "/MediaBox[[:space:]]*\\[[[:space:]]*0(?:[.]0+)?[[:space:]]+",
    "0(?:[.]0+)?[[:space:]]+([0-9.]+)[[:space:]]+([0-9.]+)[[:space:]]*\\]"
  )
  media_positions <- release_qa_raw_positions(bytes, "/MediaBox")
  media_text <- if (length(media_positions)) {
    position <- media_positions[1L]
    window <- bytes[position:min(length(bytes), position + 160L)]
    printable <- as.integer(window) %in% c(9L, 10L, 13L, 32L:126L)
    rawToChar(window[printable])
  } else {
    ""
  }
  match <- regmatches(
    media_text, regexec(media_pattern, media_text, perl = TRUE)
  )[[1L]]
  list(
    pages = pages,
    width_points = if (length(match) == 3L) as.numeric(match[2L]) else NA_real_,
    height_points = if (length(match) == 3L) as.numeric(match[3L]) else NA_real_
  )
}

release_qa_html_pages <- function(path) {
  if (!file.exists(path)) return(0L)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  matches <- gregexpr("class=[\"'][^\"']*[\"']", text, perl = TRUE)
  positions <- matches[[1L]]
  if (length(positions) == 1L && positions[1L] == -1L) return(0L)
  attributes <- regmatches(text, matches)[[1L]]
  attributes <- sub("^class=[\"']", "", attributes)
  attributes <- sub("[\"']$", "", attributes)
  sum(vapply(strsplit(attributes, "[[:space:]]+"), function(classes) {
    any(classes %in% c("sheet", "page")) &&
      !any(classes %in% c("provider-omitted", "service-omitted"))
  }, logical(1)))
}

release_qa_html_is_a4_portrait <- function(path) {
  if (!file.exists(path)) return(FALSE)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  grepl(
    "@page[[:space:]]*\\{[^}]*size[[:space:]]*:[[:space:]]*A4[[:space:]]+portrait",
    text, ignore.case = TRUE, perl = TRUE
  )
}

release_qa_import_profile <- function(national) {
  if (!nrow(national)) return(data.table::data.table())
  national[, calendar_month := data.table::as.IDate(calendar_month)]
  national[, {
    z <- .SD[complete_submission == TRUE & is.finite(value)]
    data.table::setorder(z, calendar_month)
    expected <- if (nrow(z)) data.table::as.IDate(seq(
      as.Date(min(z$calendar_month)), as.Date(max(z$calendar_month)), by = "month"
    )) else data.table::as.IDate(character())
    gaps <- setdiff(as.character(expected), as.character(z$calendar_month))
    changes <- if (nrow(z) >= 2L) diff(z$value) else numeric()
    latest_change <- if (length(changes)) utils::tail(changes, 1L) else NA_real_
    historical_changes <- if (length(changes) >= 2L) {
      utils::head(changes, -1L)
    } else {
      numeric()
    }
    centre <- if (length(historical_changes)) {
      stats::median(historical_changes, na.rm = TRUE)
    } else {
      NA_real_
    }
    scale <- if (length(historical_changes) >= 6L) {
      stats::mad(historical_changes, center = centre, na.rm = TRUE)
    } else {
      NA_real_
    }
    latest_robust_z <- if (
      is.finite(latest_change) && is.finite(centre) && is.finite(scale) && scale > 0
    ) abs(latest_change - centre) / scale else NA_real_
    list(
      first_month = if (nrow(z)) min(z$calendar_month) else data.table::as.IDate(NA),
      last_month = if (nrow(z)) max(z$calendar_month) else data.table::as.IDate(NA),
      complete_months = nrow(z),
      missing_months = length(gaps),
      missing_month_list = paste(gaps, collapse = ";"),
      source_methods = data.table::uniqueN(z$source_method),
      source_files = data.table::uniqueN(z$source_file),
      largest_absolute_monthly_change = if (length(changes)) {
        max(abs(changes), na.rm = TRUE)
      } else {
        NA_real_
      },
      latest_monthly_change = latest_change,
      latest_change_robust_z = latest_robust_z,
      latest_change_unusual = is.finite(latest_robust_z) && latest_robust_z > 6
    )
  }, by = metric_id]
}

release_qa_import_anomalies <- function(national, optional_failures) {
  parts <- list()
  part_index <- 0L
  for (metric in unique(national$metric_id)) {
    z <- data.table::copy(national[
      metric_id == metric & complete_submission == TRUE & is.finite(value)
    ])
    data.table::setorder(z, calendar_month)
    if (nrow(z) >= 2L) {
      z[, previous_value := data.table::shift(value)]
      z[, monthly_change := value - previous_value]
      largest <- utils::head(z[is.finite(monthly_change)][
        order(-abs(monthly_change))
      ], 10L)
      if (nrow(largest)) {
        part_index <- part_index + 1L
        parts[[part_index]] <- largest[, .(
          metric_id,
          calendar_month,
          anomaly_type = "largest_monthly_change",
          value,
          comparison_value = previous_value,
          difference = monthly_change,
          source_file,
          detail = "One of the ten largest absolute monthly changes in the imported series."
        )]
      }
      z[, previous_method := data.table::shift(source_method)]
      transition <- z[
        !is.na(previous_method) & source_method != previous_method
      ]
      if (nrow(transition)) {
        part_index <- part_index + 1L
        parts[[part_index]] <- transition[, .(
          metric_id,
          calendar_month,
          anomaly_type = "source_method_transition",
          value,
          comparison_value = NA_real_,
          difference = NA_real_,
          source_file,
          detail = paste0(previous_method, " -> ", source_method)
        )]
      }
    }
  }
  if (nrow(optional_failures)) {
    part_index <- part_index + 1L
    parts[[part_index]] <- optional_failures[, .(
      metric_id,
      calendar_month = data.table::as.IDate(NA),
      anomaly_type = "optional_source_rejected",
      value = NA_real_, comparison_value = NA_real_, difference = NA_real_,
      source_file,
      detail
    )]
  }
  if (!length(parts)) return(data.table::data.table(
    metric_id = character(), calendar_month = data.table::as.IDate(character()),
    anomaly_type = character(), value = numeric(), comparison_value = numeric(),
    difference = numeric(), source_file = character(), detail = character()
  ))
  data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
}

release_qa_archive_consistency <- function(
    archive, scorecard, key_columns,
    value_columns = c("predicted_value", "lower_80", "upper_80", "lower_95", "upper_95")) {
  required <- c(key_columns, value_columns)
  if (!all(required %in% names(archive)) || !all(required %in% names(scorecard))) {
    return(list(passed = FALSE, detail = "Archive or scorecard lacks required forecast columns."))
  }
  left <- archive[, c(key_columns, value_columns), with = FALSE]
  right <- scorecard[, c(key_columns, value_columns), with = FALSE]
  data.table::setnames(left, value_columns, paste0(value_columns, "_archive"))
  data.table::setnames(right, value_columns, paste0(value_columns, "_scorecard"))
  comparison <- merge(left, right, by = key_columns, all = TRUE)
  complete <- stats::complete.cases(comparison[, c(
    paste0(value_columns, "_archive"), paste0(value_columns, "_scorecard")
  ), with = FALSE])
  differences <- lapply(value_columns, function(column) {
    abs(comparison[[paste0(column, "_archive")]] -
          comparison[[paste0(column, "_scorecard")]])
  })
  changed <- if (nrow(comparison)) {
    Reduce(`|`, lapply(differences, function(x) x > 1e-12))
  } else {
    logical()
  }
  consistent <- nrow(comparison) > 0L && all(complete) && all(!changed)
  list(
    passed = consistent,
    detail = paste0(
      nrow(comparison), " archived forecast row(s); ",
      sum(!complete | changed),
      " row(s) missing or altered in the scorecard."
    )
  )
}

release_qa_interval_coverage <- function(scorecard) {
  if (!nrow(scorecard) || !all(c(
    "forecast_status", "actual_value", "lower_80", "upper_80", "lower_95", "upper_95"
  ) %in% names(scorecard))) {
    return(list(n = 0L, coverage_80 = NA_real_, coverage_95 = NA_real_))
  }
  scored <- scorecard[
    forecast_status == "scored" & is.finite(actual_value) &
      is.finite(lower_80) & is.finite(upper_80) &
      is.finite(lower_95) & is.finite(upper_95)
  ]
  list(
    n = nrow(scored),
    coverage_80 = if (nrow(scored)) mean(
      scored$actual_value >= scored$lower_80 & scored$actual_value <= scored$upper_80
    ) else NA_real_,
    coverage_95 = if (nrow(scored)) mean(
      scored$actual_value >= scored$lower_95 & scored$actual_value <= scored$upper_95
    ) else NA_real_
  )
}
